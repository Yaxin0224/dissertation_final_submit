#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
City Look Dissertation v2
stream_gvi_processor.py - Streamed GVI processing (no image saving)

Download images from Mapillary into memory, compute GVI, and only save results.
Greatly reduces storage needs, from ~100GB down to <1GB.
"""

import os
import sys
import pandas as pd
import numpy as np
from pathlib import Path
from datetime import datetime
import requests
from io import BytesIO
from PIL import Image
import torch
from tqdm import tqdm
import logging
import json
import time
from concurrent.futures import ThreadPoolExecutor
import warnings
warnings.filterwarnings('ignore')


class Config:

    BASE_DIR = Path(r"C:\Users\z1782\OneDrive - University College London\Attachments\004\methodology\Dissertation_v2")
    
    # Data paths
    IMD_DATA = BASE_DIR / "data" / "raw" / "IMD2019_London.xlsx"
    SELECTED_LSOAS = BASE_DIR / "data" / "processed" / "selected_lsoas.csv"
    
    # Output paths (results only, no images)
    OUTPUT_DIR = BASE_DIR / "output" / "stream_gvi"
    RESULTS_DIR = BASE_DIR / "data" / "processed"
    
    # Mapillary settings
    MAPILLARY_TOKEN = "MLY|9922859457805691|cef02444f32c339cf09761b104ca4bb5"
    MAPILLARY_API = "https://graph.mapillary.com"
    
    # Image collection parameters
    TARGET_IMAGES = 100  # target 100 images
    MIN_IMAGES = 60      # minimum 60 images
    MAX_IMAGES = 100     # maximum 100 images
    
    # Inner London Boroughs
    INNER_LONDON_BOROUGHS = [
        "Camden", "Greenwich", "Hackney", 
        "Hammersmith and Fulham", "Islington",
        "Kensington and Chelsea", "Lambeth", 
        "Lewisham", "Southwark", "Tower Hamlets",
        "Wandsworth", "Westminster"
    ]
    
    # Processing parameters
    BATCH_SIZE = 10      # process 10 LSOAs per batch (memory friendly)
    MAX_WORKERS = 4      # parallel download threads


# ============================================
# Mapillary streamer
# ============================================

class MapillaryStreamer:
    """Mapillary streaming image handler"""
    
    def __init__(self, token):
        self.token = token
        self.session = requests.Session()
        self.session.headers.update({
            'Authorization': f'OAuth {self.token}'
        })
        
    def search_images_in_bbox(self, bbox, limit=100):
        """Search images within a bounding box"""
        url = f"{Config.MAPILLARY_API}/images"
        
        params = {
            'bbox': f'{bbox[0]},{bbox[1]},{bbox[2]},{bbox[3]}',
            'fields': 'id,captured_at,thumb_2048_url,computed_geometry',
            'limit': min(limit, 500)  # API cap
        }
        
        try:
            response = self.session.get(url, params=params, timeout=30)
            if response.status_code == 200:
                data = response.json()
                return data.get('data', [])
        except Exception as e:
            print(f"搜索失败: {e}")
            
        return []
    
    def download_image_to_memory(self, image_url):
        """Download image into memory (do not save to disk)"""
        try:
            response = self.session.get(image_url, timeout=30)
            if response.status_code == 200:
                # Return a PIL Image object directly
                return Image.open(BytesIO(response.content)).convert('RGB')
        except Exception as e:
            print(f"下载失败: {e}")
        
        return None


# ============================================
# Lightweight GVI calculator
# ============================================

class StreamGVICalculator:
    """Streamed GVI calculator (memory-optimized)"""
    
    def __init__(self, use_gpu=False):
        self.device = torch.device('cuda' if use_gpu and torch.cuda.is_available() else 'cpu')
        print(f"使用设备: {self.device}")
        
        # Load SegFormer (lightweight)
        from transformers import SegformerForSemanticSegmentation, SegformerFeatureExtractor
        
        # Use a smaller model to save memory
        model_name = "nvidia/segformer-b0-finetuned-ade-512-512"  # b0 is smaller and faster
        self.feature_extractor = SegformerFeatureExtractor.from_pretrained(model_name)
        self.model = SegformerForSemanticSegmentation.from_pretrained(model_name)
        self.model.to(self.device)
        self.model.eval()
        
        # ADE20K vegetation classes
        self.vegetation_classes = [4, 9, 17, 29, 46, 66, 90]  # tree, grass, plant, etc.
    
    def calculate_gvi_from_pil(self, pil_image):
        """Compute GVI from a PIL image directly (no saving)"""
        try:
            # Preprocess
            inputs = self.feature_extractor(images=pil_image, return_tensors="pt")
            
            # Inference
            with torch.no_grad():
                outputs = self.model(**inputs.to(self.device))
                
                # Get prediction
                logits = outputs.logits
                pred = logits.argmax(dim=1).cpu().numpy()[0]
            
            # Vegetation mask
            vegetation_mask = np.isin(pred, self.vegetation_classes)
            
            # Compute GVI
            total_pixels = pred.size
            vegetation_pixels = np.sum(vegetation_mask)
            gvi = (vegetation_pixels / total_pixels) * 100
            
            # Free memory
            del inputs, outputs, logits, pred
            torch.cuda.empty_cache() if self.device.type == 'cuda' else None
            
            return {
                'gvi': gvi,
                'vegetation_pixels': int(vegetation_pixels),
                'total_pixels': int(total_pixels)
            }
            
        except Exception as e:
            print(f"GVI计算失败: {e}")
            return None


# ============================================
# Stream LSOA processor
# ============================================

class StreamLSOAProcessor:
    """Stream processing for a single LSOA"""
    
    def __init__(self):
        self.streamer = MapillaryStreamer(Config.MAPILLARY_TOKEN)
        self.calculator = StreamGVICalculator(use_gpu=False)  # typically CPU on Windows
        
    def process_lsoa(self, lsoa_code, bbox):
        """Process a single LSOA (without saving images)"""
        
        print(f"\n处理 {lsoa_code}...")
        
        # Step 1: search images
        images_metadata = self.streamer.search_images_in_bbox(bbox, limit=Config.MAX_IMAGES)
        
        if len(images_metadata) < Config.MIN_IMAGES:
            print(f"⚠️ {lsoa_code} 只找到 {len(images_metadata)} 张图片（需要至少{Config.MIN_IMAGES}张）")
            return None
        
        # Limit to MAX_IMAGES
        images_metadata = images_metadata[:Config.MAX_IMAGES]
        print(f"找到 {len(images_metadata)} 张图片")
        
        # Step 2: stream-process each image
        gvi_results = []
        
        with ThreadPoolExecutor(max_workers=2) as executor:  # limit concurrency to avoid OOM
            for img_meta in tqdm(images_metadata, desc=f"计算{lsoa_code}的GVI"):
                
                # Get image URL
                image_url = img_meta.get('thumb_2048_url')
                if not image_url:
                    continue
                
                # Download into memory
                pil_image = self.streamer.download_image_to_memory(image_url)
                if pil_image is None:
                    continue
                
                # Compute GVI
                gvi_result = self.calculator.calculate_gvi_from_pil(pil_image)
                if gvi_result:
                    gvi_result['image_id'] = img_meta['id']
                    gvi_result['captured_at'] = img_meta.get('captured_at', '')
                    gvi_results.append(gvi_result)
                
                # Immediately free image memory
                del pil_image
        
        # Step 3: aggregate results
        if len(gvi_results) >= Config.MIN_IMAGES:
            gvi_values = [r['gvi'] for r in gvi_results]
            
            summary = {
                'lsoa_code': lsoa_code,
                'n_images': len(gvi_results),
                'mean_gvi': np.mean(gvi_values),
                'median_gvi': np.median(gvi_values),
                'std_gvi': np.std(gvi_values),
                'min_gvi': np.min(gvi_values),
                'max_gvi': np.max(gvi_values),
                'q25_gvi': np.percentile(gvi_values, 25),
                'q75_gvi': np.percentile(gvi_values, 75),
                'processing_time': datetime.now().isoformat()
            }
            
            print(f"✅ {lsoa_code} 完成: 平均GVI={summary['mean_gvi']:.2f}%")
            
            details_df = pd.DataFrame(gvi_results)
            details_df['lsoa_code'] = lsoa_code
            
            return summary, details_df
        
        else:
            print(f"❌ {lsoa_code} 处理失败：有效图片不足")
            return None


# ============================================
# Batch stream pipeline
# ============================================

class StreamPipeline:
    """Streaming processing pipeline"""
    
    def __init__(self):
        self.processor = StreamLSOAProcessor()
        self.setup_logging()
        
    def setup_logging(self):
        """Configure logging"""
        log_dir = Config.BASE_DIR / "logs"
        log_dir.mkdir(exist_ok=True)
        
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(message)s',
            handlers=[
                logging.FileHandler(log_dir / f"stream_{datetime.now():%Y%m%d}.log"),
                logging.StreamHandler()
            ]
        )
        self.logger = logging.getLogger(__name__)
    
    def load_lsoa_boundaries(self):
        """Load LSOA boundaries (simplified placeholder)"""
        # This requires your LSOA boundary data.
        # For now return mock data.
        
        # Read IMD to get LSOA list
        imd_data = pd.read_excel(Config.IMD_DATA)
        inner_lsoas = imd_data[imd_data['Borough'].isin(Config.INNER_LONDON_BOROUGHS)]
        
        # Needs actual shapefile to get bounding boxes
        # Use simplified bbox placeholders (replace with real coordinates)
        boundaries = []
        for _, row in inner_lsoas.iterrows():
            # Example bbox [min_lon, min_lat, max_lon, max_lat]
            bbox = [-0.1, 51.5, -0.05, 51.55]  # example coords
            boundaries.append({
                'lsoa_code': row['LSOA code (2011)'],
                'bbox': bbox
            })
        
        return boundaries[:100]  # process first 100 for now
    
    def run(self, target_count=100):
        """Run the streaming pipeline"""
        
        self.logger.info(f"开始流式处理 {target_count} 个LSOA")
        
        # Load LSOA boundaries
        lsoa_boundaries = self.load_lsoa_boundaries()[:target_count]
        
        # Ensure output folder exists
        Config.OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
        
        # Process each LSOA
        all_summaries = []
        all_details = []
        
        for i, lsoa_info in enumerate(lsoa_boundaries):
            print(f"\n进度: {i+1}/{len(lsoa_boundaries)}")
            
            result = self.processor.process_lsoa(
                lsoa_info['lsoa_code'],
                lsoa_info['bbox']
            )
            
            if result:
                summary, details = result
                all_summaries.append(summary)
                all_details.append(details)
                
                # Save intermediate results every 10 LSOAs to avoid data loss
                if (i + 1) % 10 == 0:
                    self.save_intermediate_results(all_summaries, all_details)
        
        # Save final results
        self.save_final_results(all_summaries, all_details)
        
        self.logger.info("✅ 流式处理完成！")
        
        return all_summaries
    
    def save_intermediate_results(self, summaries, details):
        """Save intermediate results"""
        temp_dir = Config.OUTPUT_DIR / "temp"
        temp_dir.mkdir(exist_ok=True)
        
        pd.DataFrame(summaries).to_csv(
            temp_dir / f"temp_summary_{datetime.now():%H%M}.csv", 
            index=False
        )
        
        if details:
            pd.concat(details).to_csv(
                temp_dir / f"temp_details_{datetime.now():%H%M}.csv",
                index=False
            )
    
    def save_final_results(self, summaries, details):
        """Save final results"""
        
        # Summary CSV
        summary_df = pd.DataFrame(summaries)
        summary_df.to_csv(
            Config.RESULTS_DIR / "stream_gvi_summary.csv",
            index=False
        )
        
        # Detailed CSV (optional)
        if details:
            details_df = pd.concat(details, ignore_index=True)
            details_df.to_csv(
                Config.RESULTS_DIR / "stream_gvi_details.csv",
                index=False
            )
        
        # Print stats
        print("\n" + "="*50)
        print("📊 Processing summary:")
        print(f"  Number of LSOAs: {len(summary_df)}")
        print(f"  Mean GVI: {summary_df['mean_gvi'].mean():.2f}%")
        print(f"  GVI range: {summary_df['mean_gvi'].min():.2f}% - {summary_df['mean_gvi'].max():.2f}%")
        print(f"  Total images processed: {summary_df['n_images'].sum()}")
        print("="*50)


# ============================================
# Main
# ============================================

def main():
    """Main entry"""
    
    import argparse
    parser = argparse.ArgumentParser(description='Streamed GVI processing')
    parser.add_argument('--target', type=int, default=100,
                       help='Target number of LSOAs')
    parser.add_argument('--test', action='store_true',
                       help='Test mode (process only 5 LSOAs)')
    
    args = parser.parse_args()
    
    if args.test:
        args.target = 5
        print("🧪 测试模式：只处理5个LSOA")
    
    # Run pipeline
    pipeline = StreamPipeline()
    results = pipeline.run(target_count=args.target)
    
    print(f"\n✅ Completed! Processed {len(results)} LSOAs")
    print(f"💾 Results saved to: {Config.RESULTS_DIR}")
    print(f"📊 Storage saved: ~{args.target * 0.1:.1f}GB (compared to saving all images)")

if __name__ == "__main__":
    main()

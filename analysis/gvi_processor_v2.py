#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
City Look Dissertation v2
GVI processing script
Process images for the 20 selected LSOAs and compute GVI
"""

import os
import sys
from pathlib import Path
import pandas as pd
import numpy as np
from PIL import Image
import torch
from transformers import SegformerForSemanticSegmentation, SegformerFeatureExtractor
from tqdm import tqdm
import warnings
from datetime import datetime
warnings.filterwarnings('ignore')

# ============================================
# Configuration
# ============================================

class Config:
    """Project configuration"""
    # Base path
    BASE_DIR = Path(r"C:\Users\z1782\OneDrive - University College London\Attachments\004\methodology\Dissertation_v2")
    
    # Data file paths (confirmed by screenshots)
    IMD_FILE = BASE_DIR / "data" / "raw" / "IMD 2019" / "ID 2019 for London.xlsx"
    SELECTED_LSOAS = BASE_DIR / "analysis" / "selected_lsoas.xlsx"  # Excel format
    MAPILLARY_DIR = BASE_DIR / "data" / "raw" / "mapillary_images"
    
    # Output path
    OUTPUT_DIR = BASE_DIR / "output" / "gvi_results"
    
    # Processing parameters
    MAX_IMAGES_PER_LSOA = 100  # Max per LSOA
    TEST_MODE_IMAGES = 20  # Images per LSOA in test mode

# ============================================
# GVI Calculator
# ============================================

class GVICalculator:
    """GVI calculator"""
    
    def __init__(self, model_size='b0'):
        """
        Initialization
        model_size: 'b0' (smallest), 'b1', 'b2', etc.
        """
        print("Initializing GVI calculator...")
        self.device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
        print(f"Device: {self.device}")
        
        # Load SegFormer model
        model_name = f"nvidia/segformer-{model_size}-finetuned-ade-512-512"
        print(f"Loading model: {model_name}")
        
        try:
            self.feature_extractor = SegformerFeatureExtractor.from_pretrained(model_name)
            self.model = SegformerForSemanticSegmentation.from_pretrained(model_name)
            self.model.to(self.device)
            self.model.eval()
            print("✅ Model loaded successfully")
        except Exception as e:
            print(f"❌ Model loading failed: {e}")
            raise
        
        # Vegetation class IDs in ADE20K dataset
        # 4=tree, 9=grass, 17=plant, 29=field, 46=bush, 66=flower, 90=leaves
        self.vegetation_classes = [4, 9, 17, 29, 46, 66, 90]
    
    def calculate_gvi(self, image_path):
        """Compute GVI for a single image"""
        try:
            # Load image
            image = Image.open(image_path).convert('RGB')
            
            # Resize to speed up processing (optional)
            max_size = 512
            if max(image.size) > max_size:
                image.thumbnail((max_size, max_size), Image.Resampling.LANCZOS)
            
            # Preprocess
            inputs = self.feature_extractor(images=image, return_tensors="pt")
            inputs = {k: v.to(self.device) for k, v in inputs.items()}
            
            # Inference
            with torch.no_grad():
                outputs = self.model(**inputs)
                # Get predictions
                logits = outputs.logits
                pred = logits.argmax(dim=1).cpu().numpy()[0]
            
            # Count vegetation pixels
            vegetation_mask = np.isin(pred, self.vegetation_classes)
            total_pixels = pred.size
            vegetation_pixels = np.sum(vegetation_mask)
            
            # Compute GVI percentage
            gvi = (vegetation_pixels / total_pixels) * 100
            
            return {
                'gvi': gvi,
                'vegetation_pixels': int(vegetation_pixels),
                'total_pixels': int(total_pixels)
            }
            
        except Exception as e:
            print(f"  ⚠️  Processing failed {Path(image_path).name}: {e}")
            return None

# ============================================
# LSOA Processor
# ============================================

class LSOAProcessor:
    """Process images for an LSOA"""
    
    def __init__(self, calculator):
        self.calculator = calculator
        
    def process_lsoa(self, lsoa_code, max_images=None):
        """Process all images for a single LSOA"""
        
        # Image folder path
        image_dir = Config.MAPILLARY_DIR / lsoa_code
        
        if not image_dir.exists():
            print(f"❌ LSOA folder not found: {lsoa_code}")
            return None
        
        # List all jpg images
        image_files = list(image_dir.glob("*.jpg"))
        n_total = len(image_files)
        
        if n_total == 0:
            print(f"❌ {lsoa_code} has no images")
            return None
        
        # Limit number of images to process
        if max_images and n_total > max_images:
            image_files = image_files[:max_images]
            print(f"  Limiting images: {n_total} → {max_images}")
        
        print(f"\nProcessing {lsoa_code}: {len(image_files)} images")
        
        # Compute GVI for each image
        results = []
        for img_path in tqdm(image_files, desc=f"  {lsoa_code}", leave=False):
            result = self.calculator.calculate_gvi(img_path)
            if result:
                result['image_name'] = img_path.name
                results.append(result)
        
        if len(results) == 0:
            print(f"❌ {lsoa_code} has no successfully processed images")
            return None
        
        # Compute summary statistics
        gvi_values = [r['gvi'] for r in results]
        
        summary = {
            'lsoa_code': lsoa_code,
            'n_images_total': n_total,
            'n_images_processed': len(results),
            'mean_gvi': np.mean(gvi_values),
            'median_gvi': np.median(gvi_values),
            'std_gvi': np.std(gvi_values),
            'min_gvi': np.min(gvi_values),
            'max_gvi': np.max(gvi_values),
            'q25_gvi': np.percentile(gvi_values, 25),
            'q75_gvi': np.percentile(gvi_values, 75)
        }
        
        print(f"  ✅ Done: Mean GVI = {summary['mean_gvi']:.2f}%")
        
        return summary, results

# ============================================
# Main program
# ============================================

def main():
    """Main function"""
    import argparse
    parser = argparse.ArgumentParser(description='City Look GVI processing')
    parser.add_argument('--test', action='store_true', 
                       help='Test mode (process 20 images per LSOA)')
    parser.add_argument('--lsoas', type=int, default=None,
                       help='Number of LSOAs to process (default: all 20)')
    parser.add_argument('--model', type=str, default='b0',
                       choices=['b0', 'b1', 'b2'],
                       help='SegFormer model size')
    
    args = parser.parse_args()
    
    print("="*60)
    print("City Look - GVI Processing")
    print("="*60)
    
    # Check key paths
    print("\n📁 Checking file paths:")
    paths_to_check = [
        ("Mapillary images directory", Config.MAPILLARY_DIR),
        ("Output directory", Config.OUTPUT_DIR),
    ]
    
    for name, path in paths_to_check:
        exists = path.exists()
        print(f"  {'✅' if exists else '❌'} {name}: {path}")
    
    # Ensure output directory exists
    Config.OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    
    # Get list of LSOA folders
    print("\n📋 Retrieving LSOA list:")
    lsoa_folders = [f for f in Config.MAPILLARY_DIR.iterdir() if f.is_dir()]
    print(f"  Found {len(lsoa_folders)} LSOA folders")
    
    if len(lsoa_folders) == 0:
        print("❌ No LSOA folders found, exiting")
        return
    
    # Determine number of LSOAs to process
    if args.lsoas:
        n_process = min(args.lsoas, len(lsoa_folders))
    else:
        n_process = len(lsoa_folders)
    
    lsoa_folders = lsoa_folders[:n_process]
    
    # Determine max images per LSOA
    max_images = Config.TEST_MODE_IMAGES if args.test else Config.MAX_IMAGES_PER_LSOA
    
    print(f"\n⚙️ Processing settings:")
    print(f"  - Mode: {'Test' if args.test else 'Full'}")
    print(f"  - Number of LSOAs: {n_process}")
    print(f"  - Max images per LSOA: {max_images}")
    print(f"  - Model: SegFormer-{args.model}")
    
    # Initialize calculator
    print("\n🚀 Starting processing...")
    calculator = GVICalculator(model_size=args.model)
    processor = LSOAProcessor(calculator)
    
    # Process each LSOA
    all_summaries = []
    all_details = []
    
    for i, folder in enumerate(lsoa_folders, 1):
        print(f"\n[{i}/{n_process}]", end="")
        
        result = processor.process_lsoa(folder.name, max_images=max_images)
        
        if result:
            summary, details = result
            all_summaries.append(summary)
            
            # Save detailed results (optional)
            if args.test:  # Save details in test mode
                details_df = pd.DataFrame(details)
                details_df['lsoa_code'] = folder.name
                all_details.append(details_df)
    
    # Save results
    if all_summaries:
        print("\n\n💾 Saving results...")
        
        # Summary dataframe
        summary_df = pd.DataFrame(all_summaries)
        
        # Timestamp
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # Save summary
        summary_file = Config.OUTPUT_DIR / f"gvi_summary_{timestamp}.csv"
        summary_df.to_csv(summary_file, index=False)
        print(f"  ✅ Summary saved: {summary_file}")
        
        # Save details if available
        if all_details:
            details_df = pd.concat(all_details, ignore_index=True)
            details_file = Config.OUTPUT_DIR / f"gvi_details_{timestamp}.csv"
            details_df.to_csv(details_file, index=False)
            print(f"  ✅ Details saved: {details_file}")
        
        # Create simplified file for R integration
        r_file = Config.OUTPUT_DIR / "lsoa_gvi_summary.csv"
        summary_df.to_csv(r_file, index=False)
        print(f"  ✅ R integration file: {r_file}")
        
        # Show statistics
        print("\n📊 Processing summary:")
        print(f"  - Successfully processed: {len(summary_df)} LSOAs")
        print(f"  - Mean GVI: {summary_df['mean_gvi'].mean():.2f}%")
        print(f"  - GVI range: {summary_df['mean_gvi'].min():.2f}% - {summary_df['mean_gvi'].max():.2f}%")
        print(f"  - Std dev: {summary_df['mean_gvi'].std():.2f}%")
        
        # Top/bottom 5
        print("\n🌳 Top 5 LSOAs by GVI:")
        top5 = summary_df.nlargest(5, 'mean_gvi')[['lsoa_code', 'mean_gvi']]
        for _, row in top5.iterrows():
            print(f"  {row['lsoa_code']}: {row['mean_gvi']:.2f}%")
        
        print("\n🏢 Bottom 5 LSOAs by GVI:")
        bottom5 = summary_df.nsmallest(5, 'mean_gvi')[['lsoa_code', 'mean_gvi']]
        for _, row in bottom5.iterrows():
            print(f"  {row['lsoa_code']}: {row['mean_gvi']:.2f}%")
    
    else:
        print("\n❌ No LSOAs were processed successfully")
    
    print("\n" + "="*60)
    print("Processing complete!")
    print("="*60)

if __name__ == "__main__":
    main()

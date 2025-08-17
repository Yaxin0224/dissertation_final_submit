#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
City Look Dissertation v2 - Full version
full_london_gvi_complete.py - Integrate existing data and continue collection

Functions:
1. Integrate existing results for 171 LSOAs
2. Continue collecting data for remaining LSOAs
3. Compute GVI when n >= 30, record only when n < 30
4. Save all results into a single CSV file
"""

import os
import sys
import json
import time
import math
import logging
import warnings
from io import BytesIO
from pathlib import Path
from datetime import datetime
from concurrent.futures import ThreadPoolExecutor, as_completed

import numpy as np
import pandas as pd
import requests
from PIL import Image, ImageFile
from tqdm import tqdm

warnings.filterwarnings("ignore")
ImageFile.LOAD_TRUNCATED_IMAGES = True

try:
    import geopandas as gpd
except Exception:
    gpd = None

try:
    import torch
except Exception:
    torch = None

# ============================================
# Configuration
# ============================================

class Config:
    """Project configuration"""
    BASE_DIR = Path(r"C:\Users\z1782\OneDrive - University College London\Attachments\004\methodology\Dissertation_v2")
    
    # Data files
    IMD_FILE = BASE_DIR / "data" / "raw" / "IMD 2019" / "IMD2019_London.xlsx"
    LSOA_BOUNDARIES = BASE_DIR / "data" / "raw" / "statistical-gis-boundaries-london" / "ESRI" / "LSOA_2011_London_gen_MHW.shp"
    
    # Existing results file
    EXISTING_RESULTS = BASE_DIR / "output" / "full_london_gvi" / "inner_london_gvi_results_all_rows.csv"
    
    # Output paths
    OUTPUT_DIR = BASE_DIR / "output" / "full_london_gvi"
    MAIN_OUTPUT_FILE = OUTPUT_DIR / "inner_london_all_lsoas_status.csv"  # main output file
    CHECKPOINT_FILE = OUTPUT_DIR / "processing_checkpoint.json"
    DETAIL_DIR = OUTPUT_DIR / "image_details"
    
    # Mapillary
    MAPILLARY_TOKEN = os.getenv("MAPILLARY_TOKEN", "MLY|9922859457805691|cef02444f32c339cf09761b104ca4bb5")
    MAPILLARY_API = "https://graph.mapillary.com"
    
    # Inner London Boroughs
    INNER_LONDON_BOROUGHS = [
        "Camden", "Greenwich", "Hackney", "Hammersmith and Fulham",
        "Islington", "Kensington and Chelsea", "Lambeth", "Lewisham",
        "Southwark", "Tower Hamlets", "Wandsworth", "Westminster"
    ]
    
    # Processing parameters
    MIN_IMAGES_FOR_GVI = 30  # minimum images to compute GVI
    TARGET_IMAGES = 80        # target number of images
    MAX_IMAGES = 100          # maximum images to process per LSOA
    
    # Batching
    BATCH_SIZE = 50           # LSOAs per batch
    MAX_WORKERS = 4           # parallel worker threads
    
    # Model parameters
    MODEL_SIZE = os.getenv("SEGFORMER_SIZE", "b1" if (torch and torch.cuda.is_available()) else "b0")
    
    # Search strategy
    SEARCH_RADIUS_LEVELS = [0.0, 0.1, 0.2, 0.3, 0.5]  # bbox expansion factors
    TIME_RANGES = [
        ("2022-01-01", "2025-12-31"),
        ("2020-01-01", "2025-12-31"),
        ("2018-01-01", "2025-12-31"),
        ("2015-01-01", "2025-12-31"),
        (None, None)
    ]
    
    # Candidate column names (for reading IMD)
    IMD_LSOA_CODE_CANDS = ["LSOA code (2011)", "LSOA11CD", "lsoa11cd", "LSOA code", "LSOA_CODE"]
    IMD_LSOA_NAME_CANDS = ["LSOA name (2011)", "LSOA11NM", "lsoa11nm", "LSOA name"]
    IMD_SCORE_CANDS = ["Index of Multiple Deprivation (IMD) Score", "IMD Score", "imd_score"]
    IMD_BOROUGH_CANDS = ["Borough", "Local Authority District name (2019)", "Local Authority Name", "borough"]

# ============================================
# Logging setup
# ============================================

def setup_logging():
    Config.OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    Config.DETAIL_DIR.mkdir(parents=True, exist_ok=True)
    
    log_file = Config.OUTPUT_DIR / f"processing_{datetime.now():%Y%m%d_%H%M}.log"
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s - %(levelname)s - %(message)s",
        handlers=[
            logging.FileHandler(log_file, encoding="utf-8"),
            logging.StreamHandler(sys.stdout)
        ]
    )
    return logging.getLogger(__name__)

# ============================================
# Data manager
# ============================================

class DataManager:
    def __init__(self, logger):
        self.logger = logger
        self.all_lsoas = None
        self.results_df = None
        self.processed_codes = set()
        
    def load_existing_results(self):
        """Load existing LSOA results"""
        # Prefer the main output file (contains all processed)
        if Config.MAIN_OUTPUT_FILE.exists():
            try:
                self.results_df = pd.read_csv(Config.MAIN_OUTPUT_FILE)
                self.processed_codes = set(self.results_df['lsoa_code'].values)
                self.logger.info(f"✅ Loaded processed results: {len(self.results_df)} LSOAs")
                return True
            except Exception as e:
                self.logger.warning(f"Unable to load main file: {e}")
        
        # If main file missing, try loading the original 171 results
        if Config.EXISTING_RESULTS.exists():
            try:
                self.results_df = pd.read_csv(Config.EXISTING_RESULTS)
                self.results_df['status'] = 'completed'  # mark as completed
                self.processed_codes = set(self.results_df['lsoa_code'].values)
                self.logger.info(f"✅ Loaded existing results: {len(self.results_df)} LSOAs")
                
                # Save to main output file
                self.results_df.to_csv(Config.MAIN_OUTPUT_FILE, index=False)
                return True
            except Exception as e:
                self.logger.error(f"Failed to load existing results: {e}")
        
        self.logger.info("No existing results file found; starting from scratch")
        self.results_df = pd.DataFrame()
        return False
    
    @staticmethod
    def _pick_col(df, cands):
        """Pick an existing column name from candidates"""
        for c in cands:
            if c in df.columns:
                return c
        low = [c.lower() for c in df.columns]
        for c in cands:
            for i, name in enumerate(low):
                if c.lower() == name:
                    return df.columns[i]
        return None
    
    def load_all_lsoas(self):
        """Load all Inner London LSOAs"""
        self.logger.info("Loading all Inner London LSOAs...")
        
        if not Config.IMD_FILE.exists():
            raise FileNotFoundError(f"IMD file not found: {Config.IMD_FILE}")
        
        imd = pd.read_excel(Config.IMD_FILE)
        
        # Find correct column names
        code_col = self._pick_col(imd, Config.IMD_LSOA_CODE_CANDS)
        name_col = self._pick_col(imd, Config.IMD_LSOA_NAME_CANDS)
        score_col = self._pick_col(imd, Config.IMD_SCORE_CANDS)
        borough_col = self._pick_col(imd, Config.IMD_BOROUGH_CANDS)
        
        if not all([code_col, name_col, score_col, borough_col]):
            raise KeyError("IMD table missing required columns")
        
        # Filter Inner London
        inner = imd[imd[borough_col].isin(Config.INNER_LONDON_BOROUGHS)].copy()
        inner = inner.rename(columns={
            code_col: "lsoa_code",
            name_col: "lsoa_name",
            score_col: "imd_score",
            borough_col: "borough"
        })
        
        # Compute IMD quintiles
        inner["imd_quintile"] = pd.qcut(
            inner["imd_score"], q=5, 
            labels=["Q1_Least", "Q2", "Q3", "Q4", "Q5_Most"]
        )
        
        self.all_lsoas = inner[["lsoa_code", "lsoa_name", "borough", "imd_score", "imd_quintile"]]
        self.logger.info(f"Found {len(self.all_lsoas)} Inner London LSOAs")
        
        # Compute remaining count
        remaining = len(self.all_lsoas) - len(self.processed_codes)
        self.logger.info(f"Processed: {len(self.processed_codes)}, Remaining: {remaining}")
        
        return self.all_lsoas
    
    def get_unprocessed_lsoas(self):
        """Get unprocessed LSOAs"""
        unprocessed = self.all_lsoas[~self.all_lsoas['lsoa_code'].isin(self.processed_codes)]
        return unprocessed
    
    def save_result(self, result):
        """Save a single result"""
        # Append to DataFrame
        new_row = pd.DataFrame([result])
        
        if self.results_df is None or self.results_df.empty:
            self.results_df = new_row
        else:
            self.results_df = pd.concat([self.results_df, new_row], ignore_index=True)
        
        # Save to file
        self.results_df.to_csv(Config.MAIN_OUTPUT_FILE, index=False)
        self.processed_codes.add(result['lsoa_code'])
        
    def save_checkpoint(self):
        """Save a checkpoint"""
        checkpoint = {
            'processed': list(self.processed_codes),
            'timestamp': datetime.now().isoformat(),
            'total_processed': len(self.processed_codes)
        }
        with open(Config.CHECKPOINT_FILE, 'w') as f:
            json.dump(checkpoint, f, indent=2)

# ============================================
# Mapillary API
# ============================================

class MapillaryAPI:
    def __init__(self, token, logger):
        self.token = token
        self.logger = logger
        self.session = requests.Session()
        self.session.headers.update({"Authorization": f"OAuth {token}"})
        
    def search_images(self, bbox, min_date=None, max_date=None, limit=500):
        """Search images within a bbox"""
        url = f"{Config.MAPILLARY_API}/images"
        params = {
            "bbox": f"{bbox[0]},{bbox[1]},{bbox[2]},{bbox[3]}",
            "fields": "id,captured_at,computed_geometry,thumb_2048_url,thumb_1024_url,sequence",
            "limit": limit,
            "is_pano": "false"
        }
        
        if min_date:
            params["min_captured_at"] = f"{min_date}T00:00:00Z"
        if max_date:
            params["max_captured_at"] = f"{max_date}T23:59:59Z"
        
        try:
            resp = self.session.get(url, params=params, timeout=30)
            if resp.status_code == 200:
                data = resp.json()
                return data.get('data', [])
        except Exception as e:
            self.logger.error(f"Mapillary API error: {e}")
        
        return []
    
    def search_with_strategy(self, bbox, target=80):
        """Search images using multiple strategies"""
        all_images = []
        seen_ids = set()
        
        # Try different time ranges and spatial expansions
        for time_range in Config.TIME_RANGES:
            for expansion in Config.SEARCH_RADIUS_LEVELS:
                # Expand bbox
                expanded_bbox = self._expand_bbox(bbox, expansion)
                
                # Search
                images = self.search_images(
                    expanded_bbox,
                    min_date=time_range[0] if time_range else None,
                    max_date=time_range[1] if time_range else None
                )
                
                # Deduplicate and add
                for img in images:
                    if img.get('id') not in seen_ids:
                        all_images.append(img)
                        seen_ids.add(img.get('id'))
                
                # If enough images found, return
                if len(all_images) >= target:
                    return all_images[:target]
        
        return all_images
    
    def _expand_bbox(self, bbox, factor):
        """Expand a bounding box"""
        if factor == 0:
            return bbox
        
        center_lon = (bbox[0] + bbox[2]) / 2
        center_lat = (bbox[1] + bbox[3]) / 2
        width = bbox[2] - bbox[0]
        height = bbox[3] - bbox[1]
        
        new_width = width * (1 + factor)
        new_height = height * (1 + factor)
        
        return [
            center_lon - new_width / 2,
            center_lat - new_height / 2,
            center_lon + new_width / 2,
            center_lat + new_height / 2
        ]
    
    def download_image(self, image_meta):
        """Download a single image"""
        url = image_meta.get('thumb_2048_url') or image_meta.get('thumb_1024_url')
        if not url:
            return None
        
        try:
            resp = self.session.get(url, timeout=30)
            if resp.status_code == 200:
                return Image.open(BytesIO(resp.content)).convert('RGB')
        except Exception:
            pass
        
        return None

# ============================================
# GVI Calculator
# ============================================

class GVICalculator:
    def __init__(self, model_size="b0", device=None):
        if torch is None:
            raise ImportError("PyTorch is required")
        
        if device is None:
            device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        self.device = device
        
        # Load model
        model_name = f"nvidia/segformer-{model_size}-finetuned-ade-512-512"
        
        try:
            from transformers import AutoImageProcessor, AutoModelForSemanticSegmentation
            self.processor = AutoImageProcessor.from_pretrained(model_name)
            self.model = AutoModelForSemanticSegmentation.from_pretrained(model_name)
        except Exception as e:
            raise ImportError(f"Failed to load model: {e}")
        
        self.model.to(self.device)
        self.model.eval()
        
        # Vegetation class IDs
        self.vegetation_ids = {4, 9, 17, 29, 46, 66, 87, 90}
        print(f"Using device: {self.device}")
        print(f"Vegetation class IDs: {sorted(self.vegetation_ids)}")
    
    @torch.no_grad()
    def calculate_gvi(self, pil_image):
        """Compute GVI for a single image"""
        try:
            # Resize if necessary
            if max(pil_image.size) > 512:
                pil_image.thumbnail((512, 512), Image.Resampling.LANCZOS)
            
            # Preprocess image
            inputs = self.processor(images=pil_image, return_tensors="pt")
            inputs = {k: v.to(self.device) for k, v in inputs.items()}
            
            # Predict
            outputs = self.model(**inputs)
            pred = outputs.logits.argmax(dim=1).cpu().numpy()[0]
            
            # Compute GVI
            veg_mask = np.isin(pred, list(self.vegetation_ids))
            total_pixels = pred.size
            veg_pixels = veg_mask.sum()
            
            gvi = (veg_pixels / total_pixels) * 100.0
            
            return gvi
            
        except Exception:
            return None
        finally:
            if self.device.type == "cuda":
                torch.cuda.empty_cache()

# ============================================
# LSOA Processor
# ============================================

class LSOAProcessor:
    def __init__(self, mapillary_api, gvi_calculator, logger):
        self.mapillary = mapillary_api
        self.calculator = gvi_calculator
        self.logger = logger
    
    def process_lsoa(self, lsoa_info):
        """Process a single LSOA"""
        lsoa_code = lsoa_info['lsoa_code']
        t0 = time.time()
        
        self.logger.info(f"Processing {lsoa_code} - {lsoa_info['borough']}")
        
        # Get bbox
        bbox = self._get_bbox(lsoa_code)
        
        # Search images
        images = self.mapillary.search_with_strategy(bbox, target=Config.TARGET_IMAGES)
        n_images = len(images)
        
        self.logger.info(f"  Found {n_images} images")
        
        # Decide how to handle based on number of images
        if n_images == 0:
            # No data
            result = {
                'lsoa_code': lsoa_code,
                'lsoa_name': lsoa_info.get('lsoa_name', ''),
                'borough': lsoa_info['borough'],
                'imd_score': lsoa_info['imd_score'],
                'imd_quintile': str(lsoa_info['imd_quintile']),
                'status': 'no_data',
                'n_images': 0,
                'mean_gvi': None,
                'median_gvi': None,
                'std_gvi': None,
                'min_gvi': None,
                'max_gvi': None,
                'q25_gvi': None,
                'q75_gvi': None,
                'processing_time': time.time() - t0,
                'timestamp': datetime.now().isoformat()
            }
            
        elif n_images < Config.MIN_IMAGES_FOR_GVI:
            # Not enough images to compute GVI
            result = {
                'lsoa_code': lsoa_code,
                'lsoa_name': lsoa_info.get('lsoa_name', ''),
                'borough': lsoa_info['borough'],
                'imd_score': lsoa_info['imd_score'],
                'imd_quintile': str(lsoa_info['imd_quintile']),
                'status': 'insufficient',
                'n_images': n_images,
                'mean_gvi': None,
                'median_gvi': None,
                'std_gvi': None,
                'min_gvi': None,
                'max_gvi': None,
                'q25_gvi': None,
                'q75_gvi': None,
                'processing_time': time.time() - t0,
                'timestamp': datetime.now().isoformat()
            }
            
        else:
            # Compute GVI
            gvi_values = []
            processed = 0
            
            # Use thread pool to process images in parallel
            with ThreadPoolExecutor(max_workers=4) as executor:
                futures = []
                for img_meta in images[:Config.MAX_IMAGES]:
                    future = executor.submit(self._process_single_image, img_meta)
                    futures.append(future)
                
                # Collect results
                for future in as_completed(futures):
                    gvi = future.result()
                    if gvi is not None:
                        gvi_values.append(gvi)
                        processed += 1
                    
                    # Stop when target reached
                    if processed >= Config.TARGET_IMAGES:
                        break
            
            # Compute summary statistics
            if len(gvi_values) >= Config.MIN_IMAGES_FOR_GVI:
                gvi_array = np.array(gvi_values)
                result = {
                    'lsoa_code': lsoa_code,
                    'lsoa_name': lsoa_info.get('lsoa_name', ''),
                    'borough': lsoa_info['borough'],
                    'imd_score': lsoa_info['imd_score'],
                    'imd_quintile': str(lsoa_info['imd_quintile']),
                    'status': 'completed',
                    'n_images': len(gvi_values),
                    'mean_gvi': float(np.mean(gvi_array)),
                    'median_gvi': float(np.median(gvi_array)),
                    'std_gvi': float(np.std(gvi_array)),
                    'min_gvi': float(np.min(gvi_array)),
                    'max_gvi': float(np.max(gvi_array)),
                    'q25_gvi': float(np.percentile(gvi_array, 25)),
                    'q75_gvi': float(np.percentile(gvi_array, 75)),
                    'processing_time': time.time() - t0,
                    'timestamp': datetime.now().isoformat()
                }
                self.logger.info(f"  ✅ GVI = {result['mean_gvi']:.2f}%")
            else:
                # Insufficient processed images after attempts
                result = {
                    'lsoa_code': lsoa_code,
                    'lsoa_name': lsoa_info.get('lsoa_name', ''),
                    'borough': lsoa_info['borough'],
                    'imd_score': lsoa_info['imd_score'],
                    'imd_quintile': str(lsoa_info['imd_quintile']),
                    'status': 'insufficient',
                    'n_images': n_images,
                    'mean_gvi': None,
                    'median_gvi': None,
                    'std_gvi': None,
                    'min_gvi': None,
                    'max_gvi': None,
                    'q25_gvi': None,
                    'q75_gvi': None,
                    'processing_time': time.time() - t0,
                    'timestamp': datetime.now().isoformat()
                }
        
        return result
    
    def _process_single_image(self, img_meta):
        """Process a single image"""
        try:
            # Download image
            img = self.mapillary.download_image(img_meta)
            if img is None:
                return None
            
            # Compute GVI
            gvi = self.calculator.calculate_gvi(img)
            
            # Clean up
            img.close()
            
            return gvi
            
        except Exception:
            return None
    
    def _get_bbox(self, lsoa_code):
        """Get bounding box for an LSOA"""
        try:
            if gpd and Config.LSOA_BOUNDARIES.exists():
                gdf = gpd.read_file(Config.LSOA_BOUNDARIES)
                gdf = gdf.to_crs(epsg=4326)
                
                lsoa_row = gdf[gdf['LSOA11CD'] == lsoa_code]
                if not lsoa_row.empty:
                    bounds = lsoa_row.total_bounds
                    return [bounds[0], bounds[1], bounds[2], bounds[3]]
        except Exception:
            pass
        
        # Default bbox (central London)
        return [-0.15, 51.48, -0.05, 51.54]

# ============================================
# Main processing pipeline
# ============================================

class MainPipeline:
    def __init__(self):
        self.logger = setup_logging()
        self.data_manager = DataManager(self.logger)
        self.mapillary = MapillaryAPI(Config.MAPILLARY_TOKEN, self.logger)
        
        # Initialize GVI calculator (if available)
        try:
            self.calculator = GVICalculator(model_size=Config.MODEL_SIZE)
            self.gvi_enabled = True
        except Exception as e:
            self.logger.warning(f"GVI calculator initialization failed: {e}")
            self.logger.warning("Will only record image counts, not compute GVI")
            self.calculator = None
            self.gvi_enabled = False
        
        self.processor = LSOAProcessor(self.mapillary, self.calculator, self.logger)
    
    def run(self, batch_size=50, max_lsoas=None):
        """Run the processing pipeline"""
        self.logger.info("=" * 70)
        self.logger.info("Inner London LSOA data collection")
        self.logger.info("=" * 70)
        
        # Load existing results
        self.data_manager.load_existing_results()
        
        # Load all LSOAs
        self.data_manager.load_all_lsoas()
        
        # Get unprocessed LSOAs
        unprocessed = self.data_manager.get_unprocessed_lsoas()
        
        if max_lsoas:
            unprocessed = unprocessed.head(max_lsoas)
            self.logger.info(f"Limiting processing to: {max_lsoas} LSOAs")
        
        total_to_process = len(unprocessed)
        
        if total_to_process == 0:
            self.logger.info("All LSOAs have been processed!")
            return
        
        self.logger.info(f"To process: {total_to_process} LSOAs")
        self.logger.info(f"Batch size: {batch_size}")
        
        # Process in batches
        processed_count = 0
        
        for batch_start in range(0, total_to_process, batch_size):
            batch_end = min(batch_start + batch_size, total_to_process)
            batch = unprocessed.iloc[batch_start:batch_end]
            
            self.logger.info(f"\nBatch {batch_start//batch_size + 1}: processing {batch_start+1}-{batch_end}/{total_to_process}")
            
            for _, lsoa_info in batch.iterrows():
                processed_count += 1
                
                self.logger.info(f"\n[{processed_count}/{total_to_process}] {lsoa_info['lsoa_code']}")
                
                try:
                    # Process LSOA
                    result = self.processor.process_lsoa(lsoa_info)
                    
                    # Save result
                    self.data_manager.save_result(result)
                    
                    # Periodic checkpoint
                    if processed_count % 10 == 0:
                        self.data_manager.save_checkpoint()
                        self._report_progress()
                        
                except Exception as e:
                    self.logger.error(f"Processing failed for {lsoa_info['lsoa_code']}: {e}")
                    # Record failure
                    failed_result = {
                        'lsoa_code': lsoa_info['lsoa_code'],
                        'lsoa_name': lsoa_info.get('lsoa_name', ''),
                        'borough': lsoa_info['borough'],
                        'imd_score': lsoa_info['imd_score'],
                        'imd_quintile': str(lsoa_info['imd_quintile']),
                        'status': 'error',
                        'n_images': 0,
                        'mean_gvi': None,
                        'median_gvi': None,
                        'std_gvi': None,
                        'min_gvi': None,
                        'max_gvi': None,
                        'q25_gvi': None,
                        'q75_gvi': None,
                        'processing_time': 0,
                        'timestamp': datetime.now().isoformat()
                    }
                    self.data_manager.save_result(failed_result)
        
        # Final report
        self._final_report()
    
    def _report_progress(self):
        """Report progress"""
        df = self.data_manager.results_df
        if df is not None and len(df) > 0:
            completed = len(df[df['status'] == 'completed'])
            insufficient = len(df[df['status'] == 'insufficient'])
            no_data = len(df[df['status'] == 'no_data'])
            
            self.logger.info("\n--- Progress report ---")
            self.logger.info(f"Completed (GVI computed): {completed}")
            self.logger.info(f"Insufficient (<30 images): {insufficient}")
            self.logger.info(f"No data: {no_data}")
            self.logger.info(f"Total: {len(df)}")
    
    def _final_report(self):
        """Generate final report"""
        self.logger.info("\n" + "=" * 70)
        self.logger.info("Processing complete!")
        
        df = self.data_manager.results_df
        if df is not None and len(df) > 0:
            # Statistics
            stats = {
                'total': len(df),
                'completed': len(df[df['status'] == 'completed']),
                'insufficient': len(df[df['status'] == 'insufficient']),
                'no_data': len(df[df['status'] == 'no_data']),
                'error': len(df[df['status'] == 'error']) if 'error' in df['status'].values else 0
            }
            
            self.logger.info(f"\nFinal statistics:")
            self.logger.info(f"  Total LSOAs: {stats['total']}")
            self.logger.info(f"  GVI computed: {stats['completed']} ({stats['completed']/stats['total']*100:.1f}%)")
            self.logger.info(f"  Insufficient data: {stats['insufficient']} ({stats['insufficient']/stats['total']*100:.1f}%)")
            self.logger.info(f"  No data: {stats['no_data']} ({stats['no_data']/stats['total']*100:.1f}%)")
            
            # If GVI data exists, compute mean
            completed_df = df[df['status'] == 'completed']
            if len(completed_df) > 0:
                mean_gvi = completed_df['mean_gvi'].mean()
                self.logger.info(f"\nMean GVI: {mean_gvi:.2f}%")
            
            # Save stats report
            report_file = Config.OUTPUT_DIR / f"final_report_{datetime.now():%Y%m%d_%H%M}.json"
            with open(report_file, 'w') as f:
                json.dump(stats, f, indent=2)
            
            self.logger.info(f"\nOutput files:")
            self.logger.info(f"  Main data file: {Config.MAIN_OUTPUT_FILE}")
            self.logger.info(f"  Stats report: {report_file}")
        
        self.logger.info("=" * 70)

# ============================================
# Main entrypoint
# ============================================

def main():
    import argparse
    
    parser = argparse.ArgumentParser(description="Inner London LSOA GVI data collection")
    parser.add_argument("--test", action="store_true", help="Test mode (5 LSOAs)")
    parser.add_argument("--batch-size", type=int, default=50, help="LSOAs per batch")
    parser.add_argument("--max-lsoas", type=int, default=None, help="Maximum number of LSOAs to process")
    
    args = parser.parse_args()
    
    if args.test:
        print("🧪 Test mode: processing 5 LSOAs")
        args.max_lsoas = 5
        args.batch_size = 5
    
    print(f"Configuration:")
    print(f"  - Batch size: {args.batch_size}")
    print(f"  - Max LSOAs: {args.max_lsoas if args.max_lsoas else 'all'}")
    print(f"  - GVI calculation threshold: ≥{Config.MIN_IMAGES_FOR_GVI} images")
    print()
    
    # Run pipeline
    pipeline = MainPipeline()
    pipeline.run(batch_size=args.batch_size, max_lsoas=args.max_lsoas)

if __name__ == "__main__":
    main()

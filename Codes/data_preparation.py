import os
import glob
import pandas as pd
from PIL import Image
import pytesseract

# Path to the Tesseract executable
pytesseract.pytesseract.tesseract_cmd = (
    r"C:\Program Files\Tesseract-OCR\tesseract.exe"
)

global image_index
def generate_word_aois(image_folder, output_excel, padding=5):
    """
    Scans image folder, extracts word bounding boxes, and exports an AOI table.
    Matches Screen_name directly to R fixation Screen_name identifiers.
    """
    aoi_list = []
    
    # Get all PNG/JPG images in the directory
    image_paths = sorted(glob.glob(os.path.join(image_folder, "*.png")) + 
                         glob.glob(os.path.join(image_folder, "*.jpg")))
    
    processed_count = 0

    image_index = 1   
    for img_path in image_paths:
        filename = os.path.basename(img_path)
        
        # Skip HardB images if processing EasyB only
        if "HardB" in filename:
            continue

        # Extract Screen_name directly from filename (e.g., "EasyB_1.png" -> "EasyB_1")
        screen_name = image_index
        image_index += 1
        processed_count += 1

        # Open image
        img = Image.open(img_path)
        img_w, img_h = img.size # Image dimensions in pixels
        
        # Using "ces" for Czech language OCR
        data = pytesseract.image_to_data(img, lang='ces', output_type=pytesseract.Output.DATAFRAME)
        
        # Filter out empty text and low-confidence detections
        words_df = data[(data['conf'] > 40) & (data['text'].astype(str).str.strip() != '')].copy()
        
        for _, row in words_df.iterrows():
            word_text = str(row['text']).strip()
            
            # Tesseract gives left (x), top (y), width (w), height (h)
            x = row['left']
            y = row['top']
            w = row['width']
            h = row['height']
            
            # Bounding box bounds (Top-Left origin, Y increases downwards)
            xmin = max(0, x - padding)
            ymin = max(0, y - padding)
            xmax = min(img_w, x + w + padding)
            ymax = min(img_h, y + h + padding)
            
            aoi_list.append({
                'Screen_name': screen_name,
                'Word': word_text,
                'xmin': xmin,
                'xmax': xmax,
                'ymin': ymin,
                'ymax': ymax
            })
            
    if not aoi_list:
        print("No words extracted. Check image path or filter criteria.")
        return

    # Combine into a master Pandas DataFrame
    master_aoi_df = pd.DataFrame(aoi_list)
    
    # Save to Excel
    master_aoi_df.to_excel(output_excel, index=False)
    print(f"Successfully created AOI Table with {len(master_aoi_df)} words across {processed_count} screens!")
    print(f"Saved to: {output_excel}")

# --- RUN THE GENERATOR ---
image_directory = "C:/Users/Emir/Desktop/cvut intern/Original_Data/"
output_file = "C:/Users/Emir/Desktop/cvut intern/Original_Data/EasyB_Word_AOIs_Auto.xlsx"

generate_word_aois(image_directory, output_file, padding=5)
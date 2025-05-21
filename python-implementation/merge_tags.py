import json
import os
import sys

def merge_tags_files(temp_tags_file, output_file):
    with open(temp_tags_file, 'r') as f:
        temp_tags_data = json.load(f)
    
    if not os.path.exists(output_file):
        for tag in temp_tags_data.get('tags', []):
            tag['show'] = True
        
        with open(output_file, 'w') as f:
            json.dump(temp_tags_data, f, indent=2)
        return True
    
    with open(output_file, 'r') as f:
        existing_data = json.load(f)
    
    existing_tags_by_name = {tag['name']: tag for tag in existing_data.get('tags', [])}
    new_tags_by_name = {tag['name']: tag for tag in temp_tags_data.get('tags', [])}
    
    merged_tags = []
    
    for tag_name, tag in new_tags_by_name.items():
        if tag_name in existing_tags_by_name:
            existing_tag = existing_tags_by_name[tag_name].copy()
            existing_tag['type'] = tag['type']
            existing_tag['parent'] = tag['parent']
            existing_tag['children'] = tag['children']
            
            # Copy ancestry field from the new tags
            if 'ancestry' in tag:
                existing_tag['ancestry'] = tag['ancestry']
                
            existing_tag['show'] = True  # Set to true since it's in the new file
            merged_tags.append(existing_tag)
        else:
            new_tag = tag.copy()
            new_tag['show'] = True
            merged_tags.append(new_tag)
    
    for tag_name, tag in existing_tags_by_name.items():
        if tag_name not in new_tags_by_name:
            modified_tag = tag.copy()
            modified_tag['show'] = False
            if not any(t['name'] == tag_name for t in merged_tags):
                merged_tags.append(modified_tag)
    
    merged_data = temp_tags_data.copy()
    merged_data['tags'] = merged_tags
    
    if 'aliases' in existing_data:
        if 'aliases' not in merged_data:
            merged_data['aliases'] = {}
        for alias, value in existing_data['aliases'].items():
            if alias not in merged_data['aliases']:
                merged_data['aliases'][alias] = value
    
    if 'files' in existing_data:
        if 'files' not in merged_data:
            merged_data['files'] = {}
        for file in existing_data['files']:
            if file not in merged_data['files']:
                merged_data['files'].append(file)

    with open(output_file, 'w') as f:
        json.dump(merged_data, f, indent=2)
    
    return True

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python merge_tags.py <temp_tags_file> <output_file>")
        sys.exit(1)
    
    temp_tags_file = sys.argv[1]
    output_file = sys.argv[2]
    
    if not merge_tags_files(temp_tags_file, output_file):
        sys.exit(1)
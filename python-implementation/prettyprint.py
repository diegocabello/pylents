#!/usr/bin/env python3
import json
import sys
from collections import OrderedDict
import re

def reorder_fields(obj, is_tags=False):
    if isinstance(obj, list):
        return [reorder_fields(item, is_tags=True) if is_tags else reorder_fields(item) for item in obj]
    elif isinstance(obj, dict):
        new_obj = OrderedDict()
        
        if is_tags:
            for field in ["name", "type", "parent", "children"]:
                if field in obj:
                    if field == "children":
                        new_obj[field] = obj[field]
                    else:
                        new_obj[field] = obj[field]
            
            for key, value in obj.items():
                if key not in ["name", "type", "parent", "children"]:
                    new_obj[key] = reorder_fields(value)
        else:
            if "tags" in obj:
                new_obj["aliases"] = obj.get("aliases", {})
                new_obj["tags"] = reorder_fields(obj["tags"], is_tags=True)
            else:
                for key, value in obj.items():
                    new_obj[key] = value
                
        return new_obj
    else:
        return obj

def fix_children_format(json_str):
    lines = json_str.split('\n')
    result = []
    i = 0
    
    while i < len(lines):
        if '"children": [' in lines[i] and ']' not in lines[i]:
            # Start of a children array that spans multiple lines
            combined = lines[i].rstrip().replace('"children": [', '"children": [')
            i += 1
            
            while i < len(lines) and ']' not in lines[i]:
                content = lines[i].strip()
                if content.startswith('"') and content.endswith('",'):
                    combined += content
                elif content.startswith('"') and content.endswith('"'):
                    combined += content
                i += 1
            
            if i < len(lines):
                # Add closing bracket
                combined += lines[i].strip()
                result.append(combined)
            i += 1
        else:
            # Fix single-line empty arrays
            line = lines[i]
            if '"children": []' in line:
                line = line.replace('"children": []', '"children": []')
            # Fix completed single-line arrays
            if '"children": [' in line and ']' in line:
                pattern = r'"children": \[(.*?)\]'
                match = re.search(pattern, line)
                if match:
                    content = match.group(1).strip()
                    replacement = f'"children": [{content}]'
                    line = re.sub(pattern, replacement, line)
            result.append(line)
            i += 1
    
    # Final cleanup pass to remove any spaces between brackets and quotes
    joined = '\n'.join(result)
    joined = re.sub(r'\[ +', '[', joined)
    joined = re.sub(r' +\]', ']', joined)
    
    return joined

def pretty_print_json(input_file, output_file=None):
    try:
        with open(input_file, 'r') as f:
            data = json.load(f)
        
        reordered_data = reorder_fields(data)
        
        formatted_json = json.dumps(reordered_data, indent=2)
        formatted_json = fix_children_format(formatted_json)
        
        if output_file:
            with open(output_file, 'w') as f:
                f.write(formatted_json)
            print(f"Pretty-printed JSON saved to {output_file}")
        else:
            print(formatted_json)
            
        return True
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON in {input_file}: {e}", file=sys.stderr)
        return False
    except IOError as e:
        print(f"Error: {e}", file=sys.stderr)
        return False

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python pretty_print_json.py <input_file> [output_file]")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else None
    
    if not pretty_print_json(input_file, output_file):
        sys.exit(1)

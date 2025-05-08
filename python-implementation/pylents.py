import sys
import os
#import uuid
import json

#import xattr

def is_bidir_map_fx_operation(operation):
    if operation in ["assign", "add"]:
        return 1
    elif operation in ["remove", "rm"]:
        return 2
    else:
        return 0 # unknown

def is_visible_tag(tag):
    return 'show' not in tag or tag['show'] == True

def assign_bidir_file_tag_rel(file, tag, operation, jf):

    actual_tag_name = jf['aliases'].get(tag)

    if actual_tag_name is None:
        display_tag_name = tag
    else:
        display_tag_name = actual_tag_name
    
    foo = next((t for t in jf['tags'] if t['name'] == display_tag_name and is_visible_tag(t)), None)
        
    if not foo:
        print(f"tag or alias does not exist: {tag}")
        return

    if operation == 1:  # assign/add
        if not os.path.exists(file):
            print(f"file does not exist: {file}")
            return
        
        if foo['type'] == 'dud':
            print(f"cannot assign dud tag to files: \t{display_tag_name}")
            return
        if 'files' not in foo:
            foo['files'] = []
        if file not in foo['files']:
            foo['files'].append(file)
            print(f"assigned  file, tag: \t{file} \t{display_tag_name}")
        else:
            print(f"pre-exist file, tag: \t{file} \t{display_tag_name}")

    elif operation == 2:  # remove
        if 'files' in foo and file in foo['files']:
            foo['files'].remove(file)
            print(f"removed file, tag: \t{file} \t{tag}")
        else:
            print(f"there is no correlation between file '{file}' and tag '{display_tag_name}'")

    elif operation == 0: 
        print(f"invalid operation: {operation}")
    else:
        print("very invalid operation. something is wrong inside the code line 59")

### THESE ARE ALL RELATED

def process_tag_hierarchy(tag_name, jf, normal_and_duds, normal_tags=None):

    actual_tag_name = jf['aliases'].get(tag_name)

    if actual_tag_name is None:
        display_tag_name = tag_name
    else:
        display_tag_name = actual_tag_name
    
    tag_obj = next((t for t in jf['tags'] if t['name'] == display_tag_name and is_visible_tag(t)), None)

    #tag_obj = next((tag for tag in jf['tags'] if tag['name'] == tag_name and is_visible_tag(tag)), None)
    if not tag_obj:
        print(f"tag '{tag_name}' is not in tags")
        return
        
    collect_tags_recursively(tag_obj, jf, normal_and_duds, normal_tags)

def collect_tags_recursively(tag_obj, jf, normal_and_duds, normal_tags=None):

    if tag_obj['type'] not in ['normal', 'dud']:
        print(f"tag '{tag_obj['name']}' is of invalid type '{tag_obj['type']}'")
        return
        
    if tag_obj['name'] not in normal_and_duds:
        normal_and_duds.append(tag_obj['name'])
    
    if normal_tags is not None and tag_obj['type'] == 'normal':
        if tag_obj['name'] not in normal_tags:
            normal_tags.append(tag_obj['name'])
    
    for child_name in tag_obj['children']:
        child_obj = next((tag for tag in jf['tags'] if tag['name'] == child_name and is_visible_tag(tag)), None)
        if child_obj:
            collect_tags_recursively(child_obj, jf, normal_and_duds, 
                                    normal_tags)

def filter_command(jf, tags):
    normal_tags = []
    normal_and_duds = []
    for tag in tags:
        process_tag_hierarchy(tag, jf, normal_and_duds, normal_tags)
    
    unique_files = set()
    for tag_name in normal_tags: 
        foo = next((tag for tag in jf['tags'] if tag['name'] == tag_name and is_visible_tag(tag)), None)
        if foo and 'files' in foo:
            unique_files.update(foo['files'])
    
    for file in sorted(unique_files):
        print(file)
        # it's going to go through the json and return all the files attatched to all the tags

def inspect_command(jf, files):
    if len(files) > 1:
        multi_display = True
        tab_container = '\t'
    else: 
        multi_display = False
        tab_container = ''
    for file in files:
        if multi_display:
            header_length = max(20, len(file) + 5)
            padding = header_length - len(file)
            print(f"\n====={file}{'=' * padding}")

        for tag in jf['tags']:
            if is_visible_tag(tag) and 'files' in tag and file in tag['files']:
                print(tab_container + tag['name'])

### CLOSE RELATED

# def assign_pylents_uuid(path):
#     if os.path.isfile(path):
#         try:
#             attrs = xattr.xattr(path)
#             if b"user.pylents" not in attrs:
#                 random_id = uuid.uuid4().bytes[:7]
#                 attrs.set(b"user.pylents", random_id)
#                 print(f"Set pylents: {random_id.hex()} for {path}")
#             else:
#                 existing = attrs.get(b"user.pylents")
#                 print(f"Already has pylents: {existing.hex()} for {path}")
#         except Exception as e:
#             print(f"Error processing {path}: {e}")

def main():
    if len(sys.argv) < 3:
        print("Usage: pylents <(ttf|ftt)|fil> [(<add|remove|show> <monad> <opt1> <opt2> ...) | (<tag1> <tag2> ...)]")
        return

    command = sys.argv[1]

    with open('tags.json', 'r') as f:
        jf = json.load(f)

    if command in ["filter", "filt"]: # only filters tags right now
        tags = sys.argv[2:]
        filter_command(jf, tags)
    elif command in ["inspect", "insp"]:
        files = sys.argv[2:]
        inspect_command(jf, files)
    
    else: # catch-all for now
        if command not in ["tagtofiles", "ttf", "filetotags", 'ftt']:
            print(f"invalid command: {command}")
            return

        if len(sys.argv) >= 4:
            if is_bidir_map_fx_operation(sys.argv[2]):
                operation = is_bidir_map_fx_operation(sys.argv[2]) # this is an integer
            else:
                print(f"invalid operation: {sys.argv[2]}")
                return
            monad = sys.argv[3]
            arguments = sys.argv[4:]
        else:
            print("not enough options")
            return

        if command in ["tagtofiles", "ttf"]:

            actual_tag_name = jf['aliases'].get(monad)

            if actual_tag_name is None:
                display_tag_name = monad
            else:
                display_tag_name = actual_tag_name
            
            foo = next((t for t in jf['tags'] if t['name'] == display_tag_name and is_visible_tag(t)), None)

            #foo = next((tag for tag in jf['tags'] if tag['name'] == monad and is_visible_tag(tag)), None)
            if foo:
                if foo['type'] == 'dud':
                    print(f"cannot assign dud tag to files: \t{monad}")
                    return
                for file in arguments:
                    assign_bidir_file_tag_rel(file, monad, operation, jf)
                with open('tags.json', 'w') as f:
                    json.dump(jf, f, indent=2)
            else: 
                print(f"tag does not exist: {monad}")
        elif command in ["filetotags", "ftt"]:
            if not os.path.exists(monad):
                print(f"file does not exist: {monad}")
                return
            for tag in arguments:
                assign_bidir_file_tag_rel(monad, tag, operation, jf)
            with open('tags.json', 'w') as f:
                json.dump(jf, f, indent=2)
        else:
            print(f"error: invalid command '{command}'")

if __name__ == "__main__":
    main()

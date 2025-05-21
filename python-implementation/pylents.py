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

        elif foo['type'] == 'exclusive':
            bar = single_inspect(jf, file)
            qux = collect_tags_recursively(tag, jf)[1]
            common_elements = bar & qux

            if common_elements:
                print(f"cannot assign exclusive tag {tag} to file {file} due to children {str(common_elements)[1:-1]}")
                return

        elif foo['type'] == 'normal':
            bar = single_inspect(jf, file)
            common_elements = set(foo['ancestry']) & bar

            if common_elements:
                print(f"cannot assign normal tag {tag} to file {file} due to file {file} being assigned to ancestor exclusive tag {str(common_elements)[1:-1]}")
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

### THESE TWO ARE RELATED

def collect_tags_recursively(tag_name, jf): # this returns two lists. it makes two lists, then recursively calls a function inside it that modify those lists, then returns those lists
    #JF is passed once
    #THIS CHECKS IF EVERYTHING LOOKS LIKE IT IS GOING TO WORK IN THE TOP TAG
    actual_tag_name = jf['aliases'].get(tag_name)
    display_tag_name = actual_tag_name if actual_tag_name is not None else tag_name
    tag_obj = next((t for t in jf['tags'] if t['name'] == display_tag_name and is_visible_tag(t)), None)
    if not tag_obj:
        print(f"tag '{tag_name}' is not in tags")
        return set(), set()
    #CLOSE LAST TAG
    normal_and_duds_set, normal_tags_set = set(), set()
    
    def edit_lists(tag_object, local_normal_and_duds_set, local_normal_tags_set):
        if tag_object['type'] not in ['normal', 'dud', 'exclusive']:
            print(f"tag '{tag_object['name']}' is of invalid type '{tag_object['type']}'")
            return
        
        local_normal_and_duds_set.add(tag_object['name'])
        
        if local_normal_tags_set is not None and tag_object['type'] in ['normal', 'exclusive']:
            local_normal_tags_set.add(tag_object['name'])
        
        for child_name in tag_object['children']:
            child_object = next((tag for tag in jf['tags'] if tag['name'] == child_name and is_visible_tag(tag)), None)
            if child_object:
                edit_lists(child_object, local_normal_and_duds_set, local_normal_tags_set)
    
    edit_lists(tag_obj, normal_and_duds_set, normal_tags_set)
    
    return normal_and_duds_set, normal_tags_set


def filter_command(jf, tags):
    all_normal_tags = set()
    
    for tag in tags:
        _, normal_tags_set = collect_tags_recursively(tag, jf)
        all_normal_tags.update(normal_tags_set)
    
    unique_files = set()
    for tag_name in all_normal_tags:
        tag_obj = next((tag for tag in jf['tags'] if tag['name'] == tag_name and is_visible_tag(tag)), None)
        if tag_obj and 'files' in tag_obj:
            unique_files.update(tag_obj['files'])
    
    return sorted(unique_files)

### CLOSE RELATED


def single_inspect(jf, file):
    return_set = set()
    for tag in jf['tags']:
        if is_visible_tag(tag) and 'files' in tag and file in tag['files']:
            return_set.add(tag['name'])
    return return_set


def represent_inspect(jf, files: list):

    if len(files) > 1:
        multi_display = True
        tab_container = '\t'
    else: 
        multi_display = False
        tab_container = ''

    for count, element in enumerate([single_inspect(jf, file) for file in files]):

        if multi_display:
            header_length = max(20, len(file) + 5)
            padding = header_length - len(file)
            print(f"\n====={files[count]}{'=' * padding}")

        for tag in element:
            print(tab_container + tag)
                

### CLOSE RELATED

# file name change or movement algorithm
# 1. so now you have file name and fuzzy hash. if file name lookup returns nothing, then look up fuzzy hash. if it is close enough but it has changed, update fuzzy hash. if there are two close ones, deal with it. 
# 2if both fail, then resort to xattr (less reliable because these can get left behind)
# 3. if nothing, then say file is missing

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
        print("Usage: pylents <ttf|ftt|fil> [(<add|remove|show> <monad> <opt1> <opt2> ...) | (<tag1> <tag2> ...)]")
        return

    command = sys.argv[1]

    with open('tags.json', 'r') as f:
        jf = json.load(f)

    if command in ["filter", "fil"]: # only filters tags right now
        tags = sys.argv[2:]
        for file in filter_command(jf, tags):
            print(file.strip())
    elif command in ["inspect", "insp"]:
        files = sys.argv[2:]
        represent_inspect(jf, files)
    
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

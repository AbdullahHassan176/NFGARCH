"""
Add logging and timing to all steps in run_chronological.bat
"""
import re

def add_logging_to_step(content, step_num, start_pos):
    """Add logging to a single step"""
    # Find the step header section
    pattern = rf'(REM STEP {step_num}:.*?\n)(echo ={{{30,}}}\necho (STEP {step_num}:.*?)\necho ={{{30,}}}\necho\.)'
    
    def replacement(match):
        step_name = match.group(2).strip()
        return (
            f'{match.group(1)}'
            f'call :START_STEP "{step_name}"\n'
            f'call :LOG "Processing {step_name}"\n'
        )
    
    content, count = re.subn(pattern, replacement, content, count=1)
    if count == 0:
        return content, False
    
    # Find the corresponding error handling block and add END_STEP
    # Pattern: look for the if errorlevel block after this step
    step_end_pattern = rf'(if %errorlevel% neq 0.*?\n.*?echo \[.*?\].*?\n\) else \(\n.*?echo \[OK\].*?\n\))\n(echo\.)'
    
    def add_end_step(match):
        return f'{match.group(1)}\ncall :END_STEP\n{match.group(2)}'
    
    # Find next occurrence after the START_STEP we just added
    pos = content.find(f'call :START_STEP "STEP {step_num}:')
    if pos >= 0:
        # Search for error handling block after this position
        search_area = content[pos:pos+2000]  # Search in next 2000 chars
        if 'if %errorlevel%' in search_area:
            # Apply the END_STEP addition in this area
            before = content[:pos]
            after_match = re.sub(
                r'(\) else \(\n.*?echo \[OK\].*?\n\))(\necho\.)',
                r'\1\ncall :END_STEP\2',
                content[pos:pos+2000],
                count=1
            )
            rest = content[pos+2000:]
            content = before + after_match + rest
            return content, True
    
    return content, False

# Read the file
with open('run_chronological.bat', 'r', encoding='utf-8') as f:
    content = f.read()

# Update steps 4-22
updated = 0
for step_num in range(4, 23):
    pos = content.find(f'REM STEP {step_num}:')
    if pos >= 0:
        new_content, success = add_logging_to_step(content, step_num, pos)
        if success:
            content = new_content
            updated += 1
            print(f'✓ Updated STEP {step_num}')
        else:
            print(f'✗ Could not update STEP {step_num}')

# Write back
with open('run_chronological.bat', 'w', encoding='utf-8') as f:
    f.write(content)

print(f'\nTotal steps updated: {updated}/19')
print('File updated successfully!')

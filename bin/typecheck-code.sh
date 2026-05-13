#!/bin/bash

# Claude Code Auto-Formatting Hook
# Automatically formats source code files after Claude edits them

# Read JSON input from stdin
json_input=$(cat)

# Try to extract file path using jq if available, otherwise use grep/sed
if command -v jq &> /dev/null; then
    file_path=$(echo "$json_input" | jq -r '.tool_input.file_path // empty')
else
    # Fallback: extract file_path using grep and sed
    file_path=$(echo "$json_input" | grep -o '"file_path"[[:space:]]*:[[:space:]]*"[^"]*"' | sed 's/.*"file_path"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/')
fi

# Exit silently if no file path found or file doesn't exist
if [ -z "$file_path" ] || [ ! -f "$file_path" ]; then
    exit 0
fi

# Get file extension and basename
extension="${file_path##*.}"
basename="${file_path##*/}"

# Format based on file extension
case "$extension" in
    # JavaScript/TypeScript files - use biome, fallback to prettier
    js|jsx|ts|tsx)
        if command -v npx &> /dev/null; then
            npx tsc --no-emit "$file_path" &> /dev/null
        fi
        ;;
    # Python files - use ruff via uv tool
    py)
        if command -v uv &> /dev/null; then
            uv tool run ty check "$file_path" &> /dev/null
        fi
        ;;
esac

# Always exit successfully to avoid blocking Claude's operations
exit 0

#!/bin/bash

# Script to remind about CLAUDE.md rules
echo -e "\n\033[1;33m══════════════════════════════════════════════════════\033[0m"
echo -e "\033[1;33m                  CLAUDE REMINDER                      \033[0m"
echo -e "\033[1;33m══════════════════════════════════════════════════════\033[0m"

if [ -f "CLAUDE.md" ]; then
    echo -e "\033[1;32mCLAUDE.md found. Key rules:\033[0m\n"
    # Extract the first important rule
    head -n 8 CLAUDE.md | tail -n 6 | sed 's/^/  /'

    echo -e "\n\033[41;1;37m ✗ WRONG - DO NOT USE THIS FORMAT: \033[0m"
    echo -e "\033[1;31m  git commit -m \"Fix bug in calculation"
    echo -e "  "
    echo -e "  🤖 Generated with [Claude Code](https://claude.ai/code)"
    echo -e "  "
    echo -e "  Co-Authored-By: Claude <noreply@anthropic.com>\"\033[0m"

    echo -e "\n\033[42;1;37m ✓ CORRECT - USE THIS FORMAT: \033[0m"
    echo -e "\033[1;32m  git commit -m \"Fix bug in calculation\"\033[0m"

    echo -e "\n\033[1;31mREMEMBER: NO Claude attribution whatsoever!\033[0m"
else
    echo -e "\033[1;31mWARNING: CLAUDE.md not found!\033[0m"
    echo -e "\n\033[1;31mIMPORTANT RULE:\033[0m"
    echo -e "\033[1;31m• NO Claude attribution in commit messages\033[0m"
    echo -e "\033[1;31m• NO Co-Authored-By: Claude\033[0m"
    echo -e "\033[1;31m• NO 🤖 emojis or Claude references\033[0m"
fi

echo -e "\033[1;33m══════════════════════════════════════════════════════\033[0m\n"

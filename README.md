# 🧾 MyPandoc – Multi-format Document Converter

> Convert XML, Markdown, and JSON effortlessly using Pandoc and custom logic.

---

## 🚀 Project Overview

**MyPandoc** is a lightweight document transformation tool built around [Pandoc](https://pandoc.org/), designed to automate and simplify conversions between common structured document formats:

- 📄 **XML ↔ Markdown**
- 📄 **Markdown ↔ JSON**
- 📄 **XML ↔ JSON**

This project provides both a modular conversion pipeline and customizable scripting capabilities for batch operations or integration into other systems.

---

## 🛠️ Features

- Format detection and validation
- Bidirectional conversion between:
  - XML, Markdown, JSON
- CLI interface for ease of use
- Support for automation and scripting
- Clean, readable output formatting

---

## 📦 Tech Stack

- **Python / Bash** (optional scripting)
- **Pandoc** – universal document converter
- **JSON / XML** processing tools (`jq`, `xmlstarlet`, etc.)

---

## 📄 Usage

```bash
# Convert XML to Markdown
./mypandoc.sh input.xml -o output.md

# Convert Markdown to JSON
./mypandoc.sh input.md -o output.json

# Convert XML directly to JSON
./mypandoc.sh input.xml -o output.json

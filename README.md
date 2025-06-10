# CIPHE CMP App

## Overview

The **CIPHE CMP App** is an interactive tool designed to streamline and structure workflows for analyzing computational cytometry (CMP) data. It enables researchers to process large numbers of FCS files, integrating preprocessing, quality control, debarcoding, annotation, and prediction steps within a single user-friendly interface.

## Features

- ✅ Support for three workflow modes:
  1. Population analysis without barcoding
  2. Population analysis with barcoding
  3. Single population analysis with barcodes

- Step-by-step data loading and project management
- Preprocessing tools including normalization and FlowQC
- Debarcoding via VAEVICTIS and manual gating
- Cell population annotation using Scyan or Scaffold
- Marker prediction using pyInfinityFlow
- Built-in backup and restore functionality

---

## Accessing the App

1. Open your browser and go to:  
   `http://10.71.1.6:1234`  
   or via GoT: `10.71.1.22`

2. In **WinSCP**, upload your data:  
   Path: `/mnt/md0/CMP/input`  
   *(One folder per plate is required)*

---

## Workflow Sections

### 1. 📁 Data Management
- Define your experiment and plate numbers.
- Select FCS file paths.
- Load and upload FCS data.
- Optionally upload saved RDS backups.


### 2. 🔬 Preprocessing

**Normalization Between Plates**
- Bead gating and doublet removal.
- Visualization and clustering of bead peaks.
- Plate-wise normalization based on beads.

**FlowQC**
- Remove margin events, doublets, and unstable acquisitions using FlowCut.
- Save or revert quality control steps as needed.

**Plate Concatenation**
- Merge data from multiple plates.

**Compensation & Transformation**
- Upload compensation matrix.
- Apply transformation.
- Export transformation table.

### 3. Debarcoding
- Run **VAEVICTIS** for dimension reduction.
- Visualize, gate manually, and apply barcodes.
- Validate barcoding to enrich your dataset.

### 4. Annotation
Annotate cell populations using:
- **Scyan** with a knowledge matrix or single-population FCS files.
- **Scaffold** with CLARA clustering and scaffold map creation.

Use UMAP visualization for annotation QC before applying annotations to the full dataset.

### 5.  pyInfinityFlow
- Predict missing markers from backbone markers using xgboost.
- Upload plate-specific TXT metadata.
- Define markers, run predictions, and generate statistics.

---

## Saving and Backup

- **Save FCS files** for external analysis.
- **Save entire project** to resume analysis later.
- Save at the end of each section for best practice.

---



[CIPHE_CMP_app_tutorial.pdf](CIPHE_CMP_app-tutorial.pdf)

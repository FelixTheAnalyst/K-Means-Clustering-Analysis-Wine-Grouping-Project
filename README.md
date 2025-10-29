# 🍷 K-Means Clustering Analysis — Wine Grouping Project

## 📖 Project Overview
This project implements **K-Means clustering** from scratch in **R**, applying it to a real-world scenario for an online wine retailer from the **Western Cape region**.  
The retailer markets wines under the brand **“Rand(U)”**, where each bottle contains a wine of random origin, and identification relies on a numeric seed label (`U`).  
The goal of this analysis is to **discover natural groupings** of wines based on their **chemical and visual properties**, allowing the retailer to **brand and market** wines more effectively.

---

## ⚙️ Key Objectives
1. **Implement K-Means clustering manually** using **Lloyd’s algorithm** for a chosen number of clusters `K`.  
2. **Compute simplified silhouette scores** (without using built-in functions) to assess how well-separated the clusters are.  
3. **Compare and visualize clustering performance** across multiple values of `K` (from 1 to 6) using:
   - Within-cluster sum of squares (WCSS)
   - Average simplified silhouette scores  
4. **Interpret results** to determine:
   - The most suitable number of clusters (optimal `K`)
   - Whether the clusters represent meaningful separations among the wines

---



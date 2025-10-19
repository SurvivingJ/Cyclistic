# Coursera Capstone: Cyclistic Bike-Share Analysis

This repository contains the analysis for the Google Data Analytics Capstone project (course hosted on Coursera), focusing on the Cyclistic bike-share case study.

## Project Overview
This project analyzes a year's worth of bike-share data from Cyclistic, a fictional company operating in Chicago. The goal is to understand the key differences in behavior between its two customer segments: casual riders and annual members.

## Business Task
The primary business objective is to design a data-driven marketing strategy aimed at converting casual riders into more profitable annual members. The analysis identifies distinct usage patterns to inform targeted marketing campaigns.

## Data
The analysis uses Cyclistic's trip data from September 2024 to September 2025, which was sourced from a public Divvy dataset. The data was processed and cleaned to ensure reliability, removing duplicates, and filtering out trips with invalid durations or incomplete geographic information.

## Analysis & Key Findings
The analysis was conducted using R and the Tidyverse suite of packages. Key insights from the analysis include:
- User Breakdown: Annual members account for approximately 64% of total rides, while casual users make up the remaining 36%.
- Trip Duration: Members typically take shorter trips, averaging about 12 minutes, which suggests they use the service for regular commutes. Casual users average longer rides of around 24 minutes, indicating leisure or sightseeing use.
- Usage Patterns:
  - Member activity peaks during weekday commuter hours (mornings and late afternoons).
  - Casual rider usage is highest during afternoons and on weekends (Friday–Sunday).
- Seasonality: Ridership for both groups is highest in the summer and autumn months and drops significantly during the winter.
- Bike Preference: Classic bikes are the most popular choice for both user types, followed by electric bikes.

## Recommendations
Based on the findings, the following recommendations were proposed to help convert casual riders into members:
- Targeted Marketing: Develop marketing campaigns that highlight the convenience and cost-effectiveness of membership for short, regular trips like daily commutes.
- Promotional Offers: Implement a referral program to encourage existing members to bring in new ones.
- Value Proposition: Create messaging that quantifies the potential savings for frequent casual riders if they switch to an annual membership.

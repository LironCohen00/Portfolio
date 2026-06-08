Cloud Computing — Diets & Meals Microservices
=============================================

A containerized microservices application for managing diets, meals, and dishes.

Architecture:
- Meals/Dishes Service — RESTful API providing nutritional information for meals
  and dishes (a dish is composed of 3 meals)
- Diet Service — handles POST/GET requests for different diet types
- Database Service — MongoDB instance storing all dishes, meals, and diets
- Reverse Proxy — NGINX routing all incoming requests to the appropriate service

Each service runs in its own Docker container. The full application is
orchestrated using Docker Compose.

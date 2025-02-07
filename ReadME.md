# Eazybook - Online Book Library

Eazybook is an online book library application that allows users to browse, add to wishlist, and rent books.

## Table of Contents

- [Introduction](#introduction)
- [Architecture](#architecture)
- [Microservices](#microservices)
    - [Authentication Service](#authentication-service)
    - [Book Catalogue Service](#book-catalogue-service)
    - [Wishlist Service](#wishlist-service)
    - [User Service](#user-service)
- [Frontend](#frontend)
- [Database](#database)
- [Service Discovery](#service-discovery)
- [Deployment](#deployment)
- [Running Locally](#running-locally) (Optional - if you have instructions)
- [Technologies Used](#technologies-used)


## Introduction

Eazybook provides a platform for users to access a wide range of books online.  Users can browse the catalogue, add books to their wishlist, and rent books for a two-week period.  The application is built using a microservices architecture, leveraging Spring Boot for the backend and Vue.js for the frontend.

## Architecture

Eazybook follows a MVC microservices architecture, consisting of four distinct services: Authentication, Book Catalogue, Wishlist, and User. These services communicate with each other to provide a seamless user experience.  The frontend, built with Vue.js, interacts with these microservices. Static files are stored in an S3 bucket.
Each microservice has a controller, service and database layer. 
## Microservices

### Authentication Service

- **Responsibilities:** Central authentication and role management.
- **Functionality:**
    - Generates and manages JWT tokens.
    - Provides `/validate-token` endpoint for token validation by other services.
    - Provides `/user-role` endpoint for permission checks based on user roles.
    - Handles user login and logout.
- **Technologies:** Spring Security

### Book Catalogue Service

- **Responsibilities:** Manages book data and checkout operations.
- **Functionality:**
    - Stores book information in the database.
    - Implements Checkout Stats and Checkout Items modules.
        - Checkout Stats tracks book popularity and suggests books based on availability.
        - Checkout Items functions as a shopping cart for book rentals.
    - Enforces book rental rules (one book at a time, two-week hold).
    - Provides checkout history tracking.
- **Technologies:** Spring Boot, PostgreSQL
- **Data Storage:** S3 for static files.

### Wishlist Service

- **Responsibilities:** Manages user wishlists.
- **Functionality:**
    - Allows users to add and remove books from their wishlist.
    - Enables moving books from the wishlist to the checkout.
- **Technologies:** Spring Boot

### User Service

- **Responsibilities:** Manages user-related operations.
- **Functionality:**
    - Communicates with the Authentication service to maintain data consistency.
- **Technologies:** Spring Boot

## Frontend

- **Technology:** Vue.js
- **State Management:** Pinia
- **Deployment:** The `/dist` folder containing production files is hosted in an S3 bucket.
- **Access:** [http://ec2-52-91-196-70.compute-1.amazonaws.com/#/](http://ec2-52-91-196-70.compute-1.amazonaws.com/#/)

## Database

- **Local Deployment:** PostgreSQL in a Docker container, exposed on port 5432.
- **AWS Deployment:** PostgreSQL RDS.

## Service Discovery

- **Local Deployment:** Consul within Docker containers.
- **AWS Deployment:** Cloud Map.

## Deployment

- **AWS:**
    - Application Load Balancer distributes requests to target groups hosting each microservice.
    - Nginx acts as a reverse proxy, filtering and forwarding requests, and serving the frontend application from S3.

## Technologies Used

- **Java:** Java 17
- **Architecture:** MVC
- **Backend:** Spring Boot
- **Frontend:** Vue.js
- **Database:** PostgreSQL
- **Security:** Spring Security
- **State Management:** Pinia
- **Service Discovery:** Consul (local), Cloud Map (AWS)
- **Deployment:** AWS (EC2, RDS, S3, Cloud Map, ALB, Nginx)
- **Other:** Docker, Maven, npm, JWT
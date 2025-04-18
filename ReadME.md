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
- [Running Locally](#running-locally)
- [Technologies Used](#technologies-used)


## Introduction

Eazybook provides a platform for users to access a wide range of books online. Users can browse the catalogue, add books to their wishlist, and rent books for a two-week period. The application is built using a microservices architecture, leveraging Spring Boot for the backend and Vue.js for the frontend.

## Architecture

Eazybook follows an MVC microservices architecture, consisting of four distinct services: Authentication, Book Catalogue, Wishlist, and User. These services communicate with each other to provide a seamless user experience. The frontend, built with Vue.js, interacts with these microservices.

Each microservice has a controller, service, and database layer.


## Microservices

### Authentication Service

- **Responsibilities:** Central authentication, user data management, and token management.
- **Functionality:**
  - Stores user information in a PostgreSQL database.
  - Provides endpoints to add, retrieve, update, and delete user information.
  - Provides endpoint to retrieve user by username.
  - Handles user login.
  - Generates and manages JWT tokens.
  - Provides `/validate-token` endpoint for token validation by other services.
- **Technologies:** Spring Boot, Spring Security, PostgreSQL
- **Data Storage:** PostgreSQL
- 
### Book Catalogue Service

- **Responsibilities:** Manages book data and book stats.
- **Functionality:**
  - Stores book information in a PostgreSQL database.
  - Provides endpoints to add, retrieve, update, and delete book information.
  - Provides endpoints to retrieve all books.
  - Provides endpoints to retrieve book by id.
  - Provides endpoints to retrieve book by title.
  - Provides endpoints to retrieve book by author.
  - Provides endpoints to retrieve book by isbn.
- **Technologies:** Spring Boot, PostgreSQL
- **Data Storage:** PostgreSQL

### Wishlist Service

- **Responsibilities:** Manages user wishlists.
- **Functionality:**

  - Allows users to add books to their wishlist.
  - Allows users to remove books from their wishlist.
  - Provides endpoints to retrieve all books in a user's wishlist.
- **Technologies:** Spring Boot, PostgreSQL
- **Data Storage:** PostgreSQL

### User Service

- **Responsibilities:** Manages user data and token validation.
- **Functionality:**

  - Stores user information in a PostgreSQL database.
  - Provides endpoints to add, retrieve, update, and delete user information.
  - Provides endpoint to retrieve user by username.
  - Provides endpoint to validate user token.
  - Communicates with the Authentication service to validate user tokens.
- **Technologies:** Spring Boot, PostgreSQL
- **Data Storage:** PostgreSQL

## Frontend

- **Technology:** Vue.js
- **State Management:** Pinia
- **Deployment:** The `/dist` folder containing production files is hosted in an S3 bucket.
- **Access:** [http://eazybookapp.com/#/](http://eazybookapp.com/#/)


## Database

- **Deployment:** PostgreSQL running in a Docker container on the same droplet, exposed internally on port `5432`.

## Service Discovery

- **Technology:** [Consul](https://www.consul.io/) is used for service discovery between microservices.
- **Deployment:** Runs in a Docker container on the droplet.


## Deployment

### DigitalOcean Droplet Deployment

The entire application stack is deployed on a single DigitalOcean droplet using Docker containers.

- **Dockerized Services:**
  - Each microservice runs in its own container.
  - A dedicated PostgreSQL container handles data persistence.
  - Consul handles service registration and discovery.
  - The Vue frontend is containerized and served via Nginx.

- **Nginx:**
  - Acts as a reverse proxy.
  - Proxies requests to each microservice using container names (via internal Docker DNS).
  - Serves the frontend static files built by Vue.

- **Docker Networking:**
  - All containers are connected to the same Docker Compose network (`eazybooks_default`), allowing them to communicate via service names.

- **Access:** Application is exposed on port 80 of the droplet’s public IP address.
- 
## Technologies Used

- **Programming Languages:**
  - Java 17
- **Backend Frameworks:**
  - Spring Boot
  - Spring Security
  - Spring Data JPA
- **Frontend Frameworks:**
  - Vue.js
  - Pinia (State Management)
- **Databases:**
  - PostgreSQL
- **Communication:**
  - HTTP
  - REST
  - JSON
- **Authentication:**
  - JWT (JSON Web Tokens)
- **Testing:**
  - JUnit
  - Mockito
  - Postman
- **Service Discovery:**
  - Consul (Local)
- **Deployment:**
  - AWS (EC2, RDS, S3, Cloud Map, ALB)
  - Nginx (Reverse Proxy)
  - Docker
  - Docker Compose (Local)
- **Build Tools:**
  - Maven (Backend)
  - npm (Frontend)
- **Containerization:**
  - Docker
  - Docker Compose
- **Version Control:**
  - Git
- **Other:**
  - Openjdk
  - Alpine
  - Postgres
  - Consul
  - Docker Hub
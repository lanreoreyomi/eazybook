Absolutely — here’s an updated version of your `README.md` with **AWS deployment removed** and replaced by your **DigitalOcean Droplet deployment setup**:

---

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

---

## Introduction

Eazybook provides a platform for users to access a wide range of books online. Users can browse the catalogue, add books to their wishlist, and rent books for a two-week period. The application is built using a microservices architecture, leveraging Spring Boot for the backend and Vue.js for the frontend.

---

## Architecture

Eazybook follows an MVC microservices architecture, consisting of four distinct services: Authentication, Book Catalogue, Wishlist, and User. These services communicate with each other to provide a seamless user experience. The frontend, built with Vue.js, interacts with these microservices.

Each microservice has a controller, service, and database layer.

---

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

---

## Frontend

- **Technology:** Vue.js
- **State Management:** Pinia
- **Deployment:** Packaged with Docker and served through Nginx reverse proxy on the droplet.
- **Access:** [http://104.248.107.60](http://104.248.107.60)

---

## Database

- **Deployment:** PostgreSQL running in a Docker container on the same droplet, exposed internally on port `5432`.

---

## Service Discovery

- **Technology:** [Consul](https://www.consul.io/) is used for service discovery between microservices.
- **Deployment:** Runs in a Docker container on the droplet.

---

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

---

## Running Locally (Optional)

> _(Instructions can go here if you want users to run this locally using Docker Compose)_

---

## Technologies Used

- **Java:** Java 17
- **Architecture:** MVC
- **Backend:** Spring Boot
- **Frontend:** Vue.js
- **Database:** PostgreSQL
- **Security:** Spring Security
- **State Management:** Pinia
- **Service Discovery:** Consul
- **Deployment:** Docker on DigitalOcean Droplet
- **Other:** Docker Compose, Maven, npm, JWT, Nginx

---

Let me know if you want me to also generate a sample `docker-compose.yml` section for the full stack or instructions for setting up CI/CD for this setup!
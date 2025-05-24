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
- [Local Development Setup](#local-development-setup)
  - [Prerequisites](#prerequisites)
  - [Installation Instructions](#installation-instructions)
  - [Running the Application](#running-the-application)
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

## Local Development Setup

This section describes how to set up and run the Eazybook application locally for development purposes.

### Prerequisites

Make sure you have the following software installed on your system:

*   Java Development Kit (JDK) 17
*   Apache Maven
*   Node.js and npm
*   Docker

### Installation Instructions

*   **JDK 17:**
  *   Windows: Download from [Oracle website](https://www.oracle.com/java/technologies/javase/jdk17-archive-downloads.html) or use a package manager like Chocolatey.
  *   macOS: Download from [Oracle website](https://www.oracle.com/java/technologies/javase/jdk17-archive-downloads.html) or use Homebrew (`brew install openjdk@17`).
  *   Linux: Use a package manager (e.g., `sudo apt-get install openjdk-17-jdk` for Debian/Ubuntu, `sudo yum install java-17-openjdk-devel` for Fedora/CentOS).
*   **Apache Maven:**
  *   Windows: Download from [Maven website](https://maven.apache.org/download.cgi) and follow installation instructions. Add Maven's `bin` directory to your PATH environment variable.
  *   macOS: Use Homebrew (`brew install maven`).
  *   Linux: Use a package manager (e.g., `sudo apt-get install maven`).
*   **Node.js and npm:**
  *   Windows: Download from [Node.js website](https://nodejs.org/). npm is included with Node.js.
  *   macOS: Use Homebrew (`brew install node`).
  *   Linux: Use a package manager (e.g., `sudo apt-get install nodejs npm`) or nvm (Node Version Manager).
*   **Docker:**
  *   Windows: Download Docker Desktop from [Docker website](https://www.docker.com/products/docker-desktop).
  *   macOS: Download Docker Desktop from [Docker website](https://www.docker.com/products/docker-desktop).
  *   Linux: Follow instructions on [Docker website](https://docs.docker.com/engine/install/).

Before running `docker-compose up` for the first time, or if you want to ensure you have the latest versions of the images defined in the `docker-compose.yaml` file, you can explicitly pull them. While `docker-compose up` will automatically pull images if they are not found locally, you can also do it manually:

To pull the PostgreSQL image (the `docker-compose.yaml` uses the official 'postgres' image, typically meaning latest stable):
```bash
docker pull postgres
```

To pull the Consul image (version `1.15.3` as specified in `docker-compose.yaml`):
```bash
docker pull consul:1.15.3
```

### Running the Application

There are two primary ways to run the application locally:

1.  **Using Docker Compose (Production-like Setup):**
  *   This method runs all services (backend microservices, Postgres, Consul, Nginx, and frontend) in Docker containers, simulating a production environment.
  *   Open a terminal in the project root directory and run the command:
      ```bash
      docker-compose up -d
      ```
  *   The application will be accessible at `http://localhost`.

2.  **Running Backend and Frontend Separately (Development Setup):**
   *   This method allows for more granular control and faster development iterations for individual services.
     *   **Prerequisites:** Ensure Postgres and Consul are running. You can start them using Docker Compose:
         ```bash
         docker-compose up -d postgres consul
         ```
       ```bash
       docker run -d -p 8500:8500 consul:1.15.3
       ``` 

  *   **Backend (Spring Boot Microservices):**
    *   Each microservice (Authentication, Book Catalogue, User, Wishlist) needs to be run separately.
    *   Navigate to the root directory of each microservice (e.g., `cd authentication`).
    *   Run the service using Maven:
        ```bash
        mvn spring-boot:run
        ```
    *   Repeat this for all backend microservices.
  *   **Frontend (Vue.js):**
    *   Navigate to the `frontend` directory:
        ```bash
        cd frontend
        ```
    *   Install dependencies (if you haven't already):
        ```bash
        npm install
        ```
    *   Run the development server:
        ```bash
        npm run dev
        ```
    *   The frontend will typically be accessible at `http://localhost:5173`. Check the terminal output for the exact URL.

### Database Configuration

The application uses PostgreSQL as its database. When running the application using `docker-compose up`, a PostgreSQL container is automatically started and configured.

Here are the connection details:

*   **Database Name:** `eazybooks`
*   **Username:** `eazybook_dbuser`
*   **Password:** `eazybook_dbpassword`
*   **Host:**
  *   `postgres`: When backend services are running as Docker containers (e.g., via `docker-compose up` or if you run a backend service container manually and connect it to the `eazybooks_default` network).
  *   `localhost`: When running backend services directly on your host machine (e.g., via `mvn spring-boot:run`) and connecting to the PostgreSQL container started by `docker-compose`.
*   **Port:** `5432` (This port is mapped to the host, so it's accessible whether connecting from another container on the same Docker network or from the host machine).

These details are defined in the `docker-compose.yaml` file and are used by the backend microservices to connect to the database.

### Service Discovery (Consul)

Eazybook uses HashiCorp Consul for service discovery, allowing microservices to find and communicate with each other.

*   **How it works:** When backend microservices start, they register themselves with Consul. Other services can then query Consul to discover the network locations (IP address and port) of these registered services.
*   **Local Setup:**
  *   When running the application using `docker-compose up`, a Consul container is automatically started.
  *   If you are running services individually (e.g., for development), you can start Consul along with PostgreSQL using:
      ```bash
      docker-compose up -d postgres consul
      ```
*   **Consul UI:** You can access the Consul UI in your web browser at `http://localhost:8500`. This interface allows you to see which services are registered and their status.

When running backend services locally (e.g., via `mvn spring-boot:run`), they will automatically try to register with the Consul instance running at `http://localhost:8500`.

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
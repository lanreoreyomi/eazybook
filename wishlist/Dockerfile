FROM openjdk:17

ARG JAR_FILE=target/wishlist-0.0.1-SNAPSHOT.jar
COPY ${JAR_FILE} wishlist.jar
WORKDIR /app
EXPOSE 8017
ENTRYPOINT ["java","-jar","/wishlist.jar"]
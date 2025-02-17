FROM openjdk:17
ARG JAR_FILE=target/bookcatalogue-0.0.1-SNAPSHOT.jar
COPY ${JAR_FILE} bookcatalogue.jar
WORKDIR /app
EXPOSE 9189
ENTRYPOINT ["java","-jar","/bookcatalogue.jar"]


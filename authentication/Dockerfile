FROM openjdk:17
ARG JAR_FILE=target/authentication-0.0.1-SNAPSHOT.jar
COPY ${JAR_FILE} authenticator.jar
WORKDIR /app
EXPOSE 9084
ENTRYPOINT ["java","-jar","/authenticator.jar"]


# Build stage shared by every service; the module is selected with the MODULE arg.
FROM maven:3.9-eclipse-temurin-21 AS build
ARG MODULE
WORKDIR /workspace
COPY pom.xml .
COPY common/pom.xml common/pom.xml
COPY auth-service/pom.xml auth-service/pom.xml
COPY customer-service/pom.xml customer-service/pom.xml
COPY account-service/pom.xml account-service/pom.xml
COPY card-service/pom.xml card-service/pom.xml
COPY transaction-service/pom.xml transaction-service/pom.xml
RUN mvn -B -pl common,${MODULE} -am dependency:go-offline
COPY . .
RUN mvn -B -pl common,${MODULE} -am -DskipTests package

FROM eclipse-temurin:21-jre-jammy
ARG MODULE
WORKDIR /app
COPY --from=build /workspace/${MODULE}/target/*.jar /app/service.jar
ENTRYPOINT ["java", "-jar", "/app/service.jar"]

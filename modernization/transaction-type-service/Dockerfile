# The jar is built on the host (make build) because Maven downloads inside docker build are not reliable here.
FROM eclipse-temurin:21-jre
WORKDIR /app
COPY target/transaction-type-service-0.1.0.jar app.jar
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "/app/app.jar"]

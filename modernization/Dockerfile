# Runtime image for the batch CLI; build the jar first: mvn -B -DskipTests package
FROM eclipse-temurin:21-jre
WORKDIR /app
COPY carddemo-app/target/carddemo-app.jar /app/carddemo-app.jar
ENTRYPOINT ["java", "-jar", "/app/carddemo-app.jar"]
CMD ["--job=NOOP"]

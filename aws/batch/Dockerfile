# One image for every CardDemo batch job: docker run carddemo-batch --job=<job-name> [--param=value ...]
# Build the jar first: mvn -B verify (or mvn -B -DskipTests package)
FROM eclipse-temurin:21-jre
RUN groupadd --system carddemo && useradd --system --gid carddemo --home /app carddemo
WORKDIR /app
COPY target/carddemo-batch.jar /app/carddemo-batch.jar
USER carddemo
ENTRYPOINT ["java", "-jar", "/app/carddemo-batch.jar"]

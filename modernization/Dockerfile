# Build once from modernization/: mvn -B package -DskipTests, then docker compose build.
FROM eclipse-temurin:21-jre
ARG JAR
WORKDIR /app
COPY ${JAR} app.jar
RUN useradd --system --uid 10001 carddemo
USER carddemo
ENTRYPOINT ["java", "-XX:MaxRAMPercentage=75", "-jar", "/app/app.jar"]

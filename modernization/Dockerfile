# Build stage shared by every service; the module is selected with the MODULE arg.
FROM maven:3.9-eclipse-temurin-21 AS build
ARG MODULE
# Optional Maven Central mirror for networks that cannot reach repo1 directly.
ARG MAVEN_MIRROR_URL
WORKDIR /workspace
RUN if [ -n "$MAVEN_MIRROR_URL" ]; then \
      mkdir -p /root/.m2 && printf '%s' \
        "<settings><mirrors><mirror><id>central-mirror</id><name>central-mirror</name><url>${MAVEN_MIRROR_URL}</url><mirrorOf>central</mirrorOf></mirror></mirrors></settings>" \
        > /root/.m2/settings.xml; \
    fi
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

FROM openjdk:8-alpine

COPY target/uberjar/todo.jar /todo/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/todo/app.jar"]

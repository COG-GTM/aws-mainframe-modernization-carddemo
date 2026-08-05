package com.cog.carddemo;

public class App {
    public String greet(String name) {
        return "Hello, " + name + "!";
    }

    public static void main(String[] args) {
        System.out.println(new App().greet("CardDemo Modern"));
    }
}

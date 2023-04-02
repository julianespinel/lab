package tasks;

import java.time.LocalDateTime;

public class GetDateAndPrint implements Runnable {

    private final String name;

    public GetDateAndPrint(String name) {
        this.name = name;
    }

    @Override
    public void run() {
        System.out.println("Name: " + name + ", date: " + LocalDateTime.now());
    }
}

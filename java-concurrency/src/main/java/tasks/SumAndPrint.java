package tasks;

import java.util.Random;

public class SumAndPrint implements Runnable {

    private static final int LIMIT = 1000;
    private final String name;

    public SumAndPrint(String name) {
        this.name = name;
    }

    private long getSum() {
        Random random = new Random();
        int randomLimit = random.nextInt(LIMIT) + 1;

        long sum = 0;
        for (int i = 1; i <= randomLimit; i++) {
            sum += i;
        }

        return sum;
    }

    @Override
    public void run() {
        try {
            Thread.sleep(10);
            long sum = getSum();
            System.out.println("Name: " + name + ", sum: " + sum);
        } catch (InterruptedException e) {
            System.out.println(name + ", error: " + e.getMessage());
            Thread.currentThread().interrupt();
        }
    }
}

package tasks;

public class SleepAndPrint implements Runnable {

    private static final long ONE_SECOND = 1000;
    private final String name;

    public SleepAndPrint(String name) {
        this.name = name;
    }

    private void sayHello() {
        System.out.println("Hello, my name is " + name);
    }

    @Override
    public void run() {
        try {
            Thread.sleep(ONE_SECOND);
            Thread.sleep(ONE_SECOND);
            Thread.sleep(ONE_SECOND);
            sayHello();
            Thread.sleep(ONE_SECOND);
            Thread.sleep(ONE_SECOND);
            Thread.sleep(ONE_SECOND);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}

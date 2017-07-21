package co.je.lab.concurrency;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Created by julian on 20/07/17.
 */
public class TestScheduledTask {

    private static void scheduleTask() {
        ScheduledExecutorService scheduledExecutor = Executors.newSingleThreadScheduledExecutor();
        RunnableTask runnableTask = new RunnableTask();
        scheduledExecutor.scheduleAtFixedRate(runnableTask, 0, 1, TimeUnit.SECONDS);
    }

    public static void main(String[] args) {
        scheduleTask();
        System.out.println("************* 2");
    }
}

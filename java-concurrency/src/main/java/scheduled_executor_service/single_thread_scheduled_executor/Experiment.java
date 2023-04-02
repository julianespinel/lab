package scheduled_executor_service.single_thread_scheduled_executor;

import common.LifeCycle;
import tasks.GetDateAndPrint;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public class Experiment {

    public static void main(String[] args) throws InterruptedException {
        Thread.sleep(10000);
        System.out.println("Start");
        ScheduledExecutorService singleThreadExecutor = Executors.newSingleThreadScheduledExecutor();

        int initialDelay = 0;
        int period = 5;
        GetDateAndPrint command = new GetDateAndPrint("Thread 1");
        singleThreadExecutor.schedule(command, initialDelay, TimeUnit.SECONDS);
        singleThreadExecutor.schedule(command, 5, TimeUnit.SECONDS);
        singleThreadExecutor.schedule(command, 10, TimeUnit.SECONDS);
        singleThreadExecutor.scheduleAtFixedRate(new GetDateAndPrint("Thread 2"), initialDelay, period, TimeUnit.SECONDS);

        System.out.println("Shutdown");
        LifeCycle.shutDownExecutor(singleThreadExecutor);
        System.out.println("End");
        Thread.sleep(10000);
    }
}

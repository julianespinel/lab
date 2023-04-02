package executor_service.single_thread_executor;

import common.LifeCycle;
import tasks.SleepAndPrint;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SingleTaskExperiment {

    public static void main(String[] args) throws InterruptedException {
        Thread.sleep(10000);
        System.out.println("Start");
        ExecutorService singleThreadExecutor = Executors.newSingleThreadExecutor();
        String name = "Thread 1";
        SleepAndPrint sleepAndPrint = new SleepAndPrint(name);
        singleThreadExecutor.execute(sleepAndPrint);
        System.out.println("Shutdown");
        LifeCycle.shutDownExecutor(singleThreadExecutor);
        System.out.println("End");
        Thread.sleep(10000);
    }
}

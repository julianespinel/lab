package common;

import java.util.concurrent.ThreadPoolExecutor;

public class Stats {

    public static void printThreadPoolStats(ThreadPoolExecutor singleThreadExecutor) {
        System.out.println("queueSize: " + singleThreadExecutor.getQueue().size());
        System.out.println("activeCount: " + singleThreadExecutor.getActiveCount());
        System.out.println("taskCount: " + singleThreadExecutor.getTaskCount());
        System.out.println("completedTaskCount: " + singleThreadExecutor.getCompletedTaskCount());
    }
}

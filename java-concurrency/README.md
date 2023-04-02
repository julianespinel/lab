# Java concurrency

An experiment to illustrate java concurrency concepts.

## Description

This repository shows how to use the Java Executors API to execute `Runnable` objects asynchronously.

The project is divided in two main packages:

1. executor_service:<br>
  Contains the implementations related to the ExecutorService interface, such as:
    1. [Cached thread pool](src/main/java/executor_service/cached_thread_pool)
    1. [Fixed thread pool](src/main/java/executor_service/fixed_thread_pool)
    1. [Single thread executor](src/main/java/executor_service/single_thread_executor)
    1. [Work stealing pool](src/main/java/executor_service/work_stealing_pool)
1. scheduled_executor_service:<br>
  Contains the implementations related to the ScheduleExecutorService interface, such as:
    1. [Scheduled thread pool](src/main/java/scheduled_executor_service/scheduled_thread_pool)
    1. [Single thread scheduled executor](src/main/java/scheduled_executor_service/single_thread_scheduled_executor)
    
Inside each package you will find at least one Java class which represents an experiment using the corresponding executor.
Accompanying the Java file, there is a README.md file that describes the experiment and the results of it.

## How to run the experiments

In order to run the experiments you only have to clone the repository and run the Java class representing the experiment you want to try.

I strongly suggest you to download and start VisualVM before running the experiments. With VisualVM you can see the impact in CPU, RAM 
and created treads when you use the different executors provided by Java.


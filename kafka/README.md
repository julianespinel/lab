# Kafka lab

## Description

The program executes the following steps:

1. Create a topic.
1. Send messages to the topic.
1. Receive messages from the topic.

### How to run?

**Run Kafka with using `docker-compose`**

In a directory outside of the `lab` project execute the following steps:
1. `git clone git@github.com:wurstmeister/kafka-docker.git`
1. `cd kafka-docker/`
1. Make the following modification to the file `docker-compose-single-broker.yml`:
    ```
    -      KAFKA_ADVERTISED_HOST_NAME: 192.168.99.100
    +      KAFKA_ADVERTISED_HOST_NAME: localhost
    ```
1. `docker-compose -f docker-compose-single-broker.yml up -d`

**Run the Kafka experiment**

In the `lab` project folder execute the following steps:
1. `mvn clean test`
1. `java -jar kafka/target/kafka-1.0-SNAPSHOT-shaded.jar`

## Lessons learned

### How to add a Maven submodule

1. Create a regular Maven project. For example a project named `lab`
1. Change the packaging of `lab/pom.xml` to `<packaging>pom</packaging>`
1. `cd lab`
1. `mvn archetype:generate -DgroupId=org.baeldung  -DartifactId=<name-of-submodule>`

### Read a Kafka topic using `kafkacat`

1. `kafkacat -b localhost:9092 -t test_topic`

## Questions

### Why the classes `JsonSerializer` and `JsonDeserializer` are `public`?

I tried to make both classes `package-protected` but I got the following error:

```
Exception in thread "main" org.apache.kafka.common.KafkaException: Failed to construct kafka producer
	at org.apache.kafka.clients.producer.KafkaProducer.<init>(KafkaProducer.java:430)
	at org.apache.kafka.clients.producer.KafkaProducer.<init>(KafkaProducer.java:298)
	at com.julianespinel.kafka.Producer.getProducer(Producer.java:25)
	at com.julianespinel.App.main(App.java:39)
Caused by: org.apache.kafka.common.KafkaException: Could not instantiate class com.julianespinel.kafka.JsonSerializer
	at org.apache.kafka.common.utils.Utils.newInstance(Utils.java:315)
	at org.apache.kafka.common.config.AbstractConfig.getConfiguredInstance(AbstractConfig.java:302)
	at org.apache.kafka.clients.producer.KafkaProducer.<init>(KafkaProducer.java:368)
	... 3 more
Caused by: java.lang.IllegalAccessException: Class org.apache.kafka.common.utils.Utils can not access a member of class com.julianespinel.kafka.JsonSerializer with modifiers ""
	at sun.reflect.Reflection.ensureMemberAccess(Reflection.java:102)
	at java.lang.reflect.AccessibleObject.slowCheckMemberAccess(AccessibleObject.java:296)
	at java.lang.reflect.AccessibleObject.checkAccess(AccessibleObject.java:288)
	at java.lang.reflect.Constructor.newInstance(Constructor.java:413)
	at org.apache.kafka.common.utils.Utils.newInstance(Utils.java:311)
	... 5 more

Process finished with exit code 1
```

Apparently the Kafka library tries to do reflection over the classes but it can't because the classes can be accessed only from the same
package where they are located.
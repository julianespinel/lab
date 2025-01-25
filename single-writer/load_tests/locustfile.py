from locust import HttpUser, task, between, events
import random
import json
import logging
import time

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("voting-load-test")

# Add test start and stop handlers
@events.test_start.add_listener
def on_test_start(environment, **kwargs):
    logger.info("Load test is starting")

@events.test_stop.add_listener
def on_test_stop(environment, **kwargs):
    logger.info("Load test is stopping")

class VotingUser(HttpUser):
    wait_time = between(2, 4)  # Random wait (in seconds) between tasks
    _user_id = 0  # Class variable to keep track of user IDs
    
    def on_start(self):
        # Initialize user's state
        VotingUser._user_id += 1
        self.user_number = VotingUser._user_id
        self.user_votes_count = 0
        logger.info(f"User #{self.user_number} started")

    def on_stop(self):
        logger.info(f"User #{self.user_number} stopped. Total votes cast: {self.user_votes_count}")

    @task
    def vote_flow(self):
        start_time = time.time()
        flow_success = True
        
        # 1. Cast a vote
        option = f"option{random.randint(1, 3)}"
        logger.debug(f"User #{self.user_number} attempting to vote for {option}")
        
        with self.client.post(
            "/votes",
            json={"option": option},
            catch_response=True
        ) as response:
            if response.status_code == 202:
                vote_data = response.json()
                self.user_votes_count += 1
                logger.info(f"User #{self.user_number} successfully queued vote for {option}")
            else:
                flow_success = False
                error_msg = f"Failed to queue vote. Status: {response.status_code}, Response: {response.text}"
                logger.error(f"User #{self.user_number}: {error_msg}")
                response.failure(error_msg)

        # 2. Check vote totals
        logger.debug(f"User #{self.user_number} checking vote totals")
        with self.client.get("/votes", catch_response=True) as response:
            if response.status_code == 200:
                totals = response.json()
                if totals:
                    logger.info(f"Current vote totals: {json.dumps(totals)}")
                else:
                    flow_success = False
                    logger.error(f"User #{self.user_number}: Vote totals empty")
                    response.failure("Vote totals empty")
            else:
                flow_success = False
                error_msg = f"Failed to get totals. Status: {response.status_code}, Response: {response.text}"
                logger.error(f"User #{self.user_number}: {error_msg}")
                response.failure(error_msg)

        # 3. Check for a random vote
        vote_id = random.randint(1, self.user_number)
        logger.debug(f"User #{self.user_number} checking vote ID {vote_id}")
        
        with self.client.get(
            f"/votes/{vote_id}",
            catch_response=True,
            name="/votes/[vote_id]",
        ) as response:
            if response.status_code == 200:
                logger.info(f"User #{self.user_number} successfully retrieved vote {vote_id}")
            elif response.status_code == 404:
                # Not finding a random vote is acceptable
                logger.debug(f"User #{self.user_number}: Vote {vote_id} not found")
            else:
                flow_success = False
                error_msg = f"Failed to get vote. Status: {response.status_code}, Response: {response.text}"
                logger.error(f"User #{self.user_number}: {error_msg}")
                response.failure(error_msg)

        # Log flow completion
        duration = time.time() - start_time
        if flow_success:
            logger.info(f"User #{self.user_number} completed flow in {duration:.2f}s")
        else:
            logger.error(f"User #{self.user_number} flow failed after {duration:.2f}s") 
#!/bin/bash

# Copy mp8 directory to Docker container
docker cp mp9 coms342:/

# Start the Docker container
docker start coms342

# Run commands inside Docker container
docker exec coms342 /bin/bash -c "cd mp9 && make clean && make && ./grader"

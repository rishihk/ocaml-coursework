# Use an official Ubuntu as a parent image
FROM ubuntu:latest

# Update and install OPAM
RUN apt-get update && apt-get install -y opam

# Initialize OPAM
RUN opam init -y

# Set up environment for OPAM
ENV OPAMYES=1
RUN opam switch create 4.13.1
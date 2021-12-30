# erl_playground

An OTP application to start coding without the boring stuff.

## Prerequisites
This project has been written for Mac and Linux environments, theoretically speaking it can run on any environment where a Erlang system is correcty installed, but consider that MS Windows and Erlang are not best buddies. Nowadays it is pretty easy to have Linux systems running in minutes using Virtual Machines, Containers, USB distro or simply double booting your laptop.

In case you use a Mac system, we strongly recommend using [homebrew](https://brew.sh/) to manage all your packages.

**OpenSSL**

Check the correct installation process for you environment.

**Erlang/OTP 21.3**

If you are on Mac, we strongly suggest using [kerl](https://github.com/kerl/kerl) to build and install the proper Erlang version on your system. For other environments you can easily find your installation package on [ErlangSolutions](https://www.erlang-solutions.com/).

## Build & Run

This is a [rebar3](https://www.rebar3.org/) project.

## Compile GPB

Google Protocol Buffer is automatically compiled starting from the included proto file.
[Here](https://developers.google.com/protocol-buffers/) you can find all the information about it.

## What you have out of the box
This is a playgrounf application that allows you to focus on the logic of your system, rather than the boring technical stuff. It includes a basic Erlang/OTP application structure with a TCP client and a TCP server.

# Candidate comments
The implemented solution contains an automatic responder with three distinct functionalities:
1. Request of CallerID
2. Request for the joke of the day
3. Chat with an operator

# 1. CallerID
At start time, the application assigns a Unique caller ID to the user and when it is asked to show this unique ID, it prints the identifier in the terminal.

# 2. Joke of the day
The application will return the joke of the current day, picking from a pool of jokes contained in the file 'jokes.txt'. It shows a different joke based on the system current date. To test this functionality is sufficient to change your system clock :)

# 3. Chat with an operator
When the user ask to chat with an operator, a greeting message is shown and the user can start sending messages.
The operator has a limited number of interactions and when he reaches this limit, he disconnects from the chat and sends the user back to the menu.
When the greetings message is shown, a timer is started and if the application does not receive any input in a short period, the operator disconnects from the chat. In this case the user remains connected to the chat and it is necessary to type the word 'bye' to return to the main menu.
Timeout seconds and the maximum number of requests can be configured in the sys.config file (parameters 'maxSeconds' and 'maxMsg').

# RUN
To start the application:
1. open a terminal in the 'challenge-call-center' folder;
2. type the compilation command 'rebar3 shell';
3. launch the command 'client:run()'.
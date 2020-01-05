---
layout: post
title: Scotty Tutorial Outline and Goals
date: 2020-01-01
description: Design intent for the tutorial and the application we are building.
img: Haskell.png # Add image post (optional)
fig-caption: # Add figcaption (optional)
tags: [Scotty, Haskell]
---

I started learning Haskell like many others did with [Learn You a Haskell][Learn-You-A-Haskell-For-Great-Good] and I quickly went over to [The Haskell Book][the-haskell-book]. I think the Haskell book is one fo the best software books I've ever read (I was only able to make to Chapter 24, didn't have the mental fortitude to keep reading I was antsy to do something). After the haskell book while i thought I had a good understanding of haskell I found that there was a lack of intermediate level resources for wannabe Haskellers. I was looking to use haskell for web development but the vast majority of resources I found were in two categories ***Trivial how to setup "x" and not much else*** or ***Here is a fully working example of a server on GitHub utiilizing Monad Transformers, Dependent Types, etc*** there was nothing that showed how you could start simple and then gradually reach for more complex abstractions as the applications needs changed. I'm not saying this to knock any of the writers of these resources this was just a personal observation. 

Luckily I've been able to piece together pieces of the information that I've found and was able to better my understanding of Application Design with Haskell. So I've decided to try to make a tutorial that I would've liked to have been able to read in the past. Now I definitely wouldn't consider myself to be a haskell expert and I don't think I ever will be so if you see something wrong please let me know and I will make an effort to fix it. I intend for this to be a WIP and will update resources as I see fit. Hopefully I am able to provide someone that is learning Haskell with some information. I would also appreciate critiques to my writing style as well. Ok let's get on to what we will actually be building, the libraries we will use and the structure of the project.

**Technologies:**
- SendGrid
- AWS S3
- PostgreSQL
- Redis
- JWT
- OAuth2
- Haddock
- Swagger

**Notable Libraries:**
- scotty
- postgresql-simple	
- postgresql-simple-migration
- amazonka
- sendgrid-v3
- jose	
- hedis
- wreq
- fast-logger


What I will try to do is start simple and gradually add complexity to the application. We will try to design this with as few language extensions as possible. I would like to start off with a more verbose way of introducing topics and towards later parts of the tutorial introduce topics with less prose and more code.

Our Project will take on a Port Adapter style architecture also referred to as hexagonal architecture or core shell architecture. My plan is to structure the tutorial in the following fashion.

**Project Structure:**
* Part 1: JSON Validation and web server setup
* Part 2: Adding state to the application with PostgreSQL
* Part 3: Adding Authentication to the application via JWT security (Maybe Session)
* Part 4: Adding Testing to the application.
* Part 5: Adding Logging to the Application
* Part 6: Adding Email Verification with SendGrid.
* Part 7: Creating Better Error with Applicative JSON validation.
* Part 8: Adding Image Upload with AWS S3 and amazonka.
* Part 9: Refactoring our application to a Monad Transformer Stack utilizing the ReaderT design pattern
* Part 10: Adding OAuth to our application
* Part 11: Adding Caching to our application
* Part 12: Deployment

This is my first try at writing a tutorial so bear with me as I get better at it and please provide whatever criticism you think would increase the quality of the tutorial.

The site will be built using stack to ease development. I assume you have stack installed in the first part of the tutorial.

With that being said head over to [(Part 1)]({{site.baseurl}}/scotty-tutorial-part-1/)

[Learn-You-A-Haskell-For-Great-Good]: http://learnyouahaskell.com/
[the-haskell-book]: https://haskellbook.com/
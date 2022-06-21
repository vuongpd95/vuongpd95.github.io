---
layout: post
title:  "What I learned from the book Test Driven Development: By Examples - Kent Beck"
date:   2022-06-20 12:38:00 +0700
categories: books
---
Kent Beck is a famous figure in the software development world. He is one of the 17 original signatories of the Agile Manifesto - the founding document for agile software development. He is one of the fathers of Extreme Programming - A software development methodology closely associated with Test-Driven Development (TDD). I was looking for materials to learn TDD and stumbled upon one of his books, which many people recommend: Test-Driven Development: By Example. This post focuses on everything that I have learned or unlearned after reading his book.

Readers are advised to go through [the book][tdd-kent-beck] first to get a sense of what I am going to write about. Here is [the codebase][the-codebase] for the first half of the book.

#### 1. The test file is my design ground.

While following the book, I find myself writing tests first as the gurus teach it. The process includes assuming I already have a particular class or method. Then, I decide how I will use them in an actual codebase without them having an actual implementation. This process is similar to the process of designing. Design, as the dictionary states, is to decide upon the look and functioning of a building, garment, or other objects, by making a detailed drawing of it.

I come to ask myself a question, how many design grounds have I used when I do programming?

The first design ground is my head. I design things in my head, using my imagination. This design ground requires no external tools. I can use it anywhere, anytime, and with little effort.

The second design ground is the paper or any surface I can write on. I use it when I find it difficult to keep track of every component of the system in my head. The language for this design ground can be as informal as basic blocks, arrows, and labels. Or, it can be as formal as UML (Unified Modeling Language). The objectives are visualizing and communicating a problem, a procedure, or a system.

The test file in that sense is also a design ground and a more sophisticated one at that. When I look more closely at the way the author guides the learning of TDD, he encourages me to put my thoughts down to the test file, which means using the test file to complement my thinking process. In doing so, the thought becomes tangible and easier to navigate.

The test file is a more sophisticated and useful design ground since it helps the develoment of software in the long term. It’s runnable. It breaks when the system changes, which reminds people to keep it up-to-date. It’s smarter than a UML drawing. Unlike a UML drawing, a new test file can run against the current system and developers can get a sense of what the system is having or missing. There are certainly more use-cases that a test file can do better than a UML drawing.

Incorporating the use of test files into our familiar ways of designing requires effort because compared to traditional ways of using thinking and using papers, managing tests are harder and more complicated.

#### 2. Knowing how to write tests doesn't mean knowing how to write good codes or how to create good designs.

Before reading (or buying) a book, I like to read its reviews. There is a significant amount of high-quality reviews on Amazon for famous books. But, this [one review][a-representative-review] comes back to my mind after finishing Kent Beck's book.

> The main example, taking the first half of the book, inexplicably uses "expressions" as objects for converting money. Why would a sane person conceive of this approach? Mid-book, at the end of the example, the author finally solves this mystery by noting that he has written the program twenty or thirty times in different formats and has reworked the book to shoehorn in his latest "metaphor" for the problem. Okay, fine, this is programming as refinement and art, but the lack of a logical approach - or rather the jump to an approach that occurred to the author after thirty rewrites - is too jarring and distracts from what he is trying to present. Also, he is slowing things down and taking small test, develop, refine steps for his readers. However, the steps are so small that you question his - and thus TDD's - ability to accomplish anything.

The person who wrote the above review and many other reviewers were perplexed at how Kent Beck can arrive at such an elegant solution for the Money problem. It doesn't make any sense. Kent Beck also didn't elaborate much on that either.

I agree with these reviews. I was also having the same question while reading his book. The tests lead to the implementation of the Money objects. It decides what part of the codes will be added next and in what amount. But the tests don't create the overall design of the Money objects. It's Kent Beck's expertise that created it. Also, it would be an even bigger distraction to write about how he came up with the design of Money since the book is not about designing software but about TDD.

Understanding that it's the developer who carries the burden of designing is very important because it's a prime example telling me TDD is not a silver bullet. TDD is not something that can replace my learning of design patterns and domain modeling. TDD is the steering wheel and the gas pedal of a car, they only aid the driver.

#### 3. Scaling the codebase by eliminating fear.

It's not easy to lay a foundation for a clean codebase. It's even harder to maintain the codebase health through time and after generations of developers. Every codebase starts small. The developers won't leave their job anytime soon, and everyone knows what every piece of code does. The codebase is in a healthy state. But, it comes to my attention that a healthy state requires many things done right.

After a few years, the codebase grows bigger and bigger and becomes too complicated to understand. Nobody dares to make significant changes because the change might break things. The team of developers won't be the same anymore. Even the developer who stays will forget what some of his codes do. At one point, the development speed will slow down to a crawl because of the fear and technical debt piled up. Finally, it stops moving.

At first, I turned to design patterns and best practices. They are what developers use to communicate and limit technical debt. With a set of great patterns and best practices setup, I hoped my team would be able to keep the codebase clean. It was not as simple as I expected. All kinds of problems cropped up:

- Developers introduce and follow design patterns and best practices incorrectly, not just because they are not good enough, but because applying design patterns is hard. It's simply easy to make mistakes.

- Unexpected changes in software specification and delivery timeline make doing things properly harder.

Design pattern helps, but it's also a double-edged sword. You know how to use it. Great, you have a great tool at your disposal. But, if you don't, you can speed up the meeting with disaster.

I feel that TDD can be a new useful tool for my toolbox. It doesn't directly solve the problem of scaling a codebase. But, it keeps the door for changes open. The test tells what the code does. It keeps the fear of making changes away. It gives developers time to learn and refactor the codebase back to its healthy state while keeping the business running with fewer bugs. On the otherhand, I don't think TDD will be much different from design pattern. I will need to learn and master it. Otherwise, I will only double the work I need to do.

#### 4. To write only the codes needed to pass the tests.

I remember the time when I first started studying and applying design patterns. The codebase for one of my projects became too complicated because I overused design patterns. Over-engineering is a super real problem in the software development world. I have come a long way and always remember to keep things simple.

Writing only the codes needed to pass the tests is a step up from remembering to keep things simple. It's simply much more actionable.

#### 5. If dependency is the problem, duplication is the symptom.

It's always a pleasant surprise when the author is writing about one thing, and he suddenly muses about something unrelated, but you can still learn from it.

While trying to remove the `Dollar` class and `Franc` class from the test file, Kent Beck mentioned:

> If dependency is the problem, duplication is the symptom.

I was surprised by this statement. Shouldn't all duplicated code pieces have the same dependencies and use the same symbols (classes)? Removing duplications shouldn't help with the dependency problem. 
Then, I realized after considering the context, that Kent Beck was talking about replacing the `Dollar` class and `Franc` class with the `Money` class. He considered these two classes duplications. In Kent Beck's mind, not only are similar pieces of code duplications but also the codes which serve the same purpose or give access to the same system duplications. This is very much aligned with what I have learned recently about Domain-Driven Design.
Even though, these are just speculations on my part about how Kent Beck thinks. It has given me a new valuable perspective on looking at code duplications.

[tdd-kent-beck]: https://www.amazon.com/Test-Driven-Development-Kent-Beck/dp/0321146530/ref=cm_cr_arp_d_product_top?ie=UTF8
[a-representative-review]: https://www.amazon.com/gp/customer-reviews/RO5VSMN102B4F/ref=cm_cr_arp_d_rvw_ttl?ie=UTF8&ASIN=0321146530
[the-codebase]: https://github.com/vuongtw/test-java

---
title: Is your codes tightly-coupled?
author: Vuong
---

People often said that it's a bad practice to write tightly-coupled codes. It's
rather quick & easy to make a comment like this but to effectively apply this
principle in practice, there are a few questions we need to answer:

1. How do we even spot tightly-coupled codes?
2. Why is tightly-coupled codes bad?
3. What can we do about it?

Let's look at a few example of coupled codes. They are not necessary
tightly-coupled but over using this way of programming can easy makes a program
tightly-coupled.

```ruby
class A
    def initialize
        @b = B.new
    end

    def perform
        puts b.data[0]
    end
end

class B
    def initialize
        @data = [1, 2, 3]
    end

    def data
        @data
    end
end

a = A.new
a.perform
```

Above is an arbitrary Ruby example of 2 coupled classes A & B. Let's look 
at their visual representation to see why it's so

![Coupled A and B representation](/images/coupled-A-B.png)

How to read the visualization

- The retangle shapes represent a class or an instance of that class
- The diamond shapes represent methods. Since `[]` and `first` are methods we
can call from `data`, they are inside the data retangle. `b` is being used in
`perform` method so it's inside the perform diamond.
- When the perform method does `b.data[0]`, we draw the red arrows.

Now since we can draw 3 arrows from `perform` to `[]`, we say A is coupled to
B with the intensity of 3

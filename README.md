**CSCI 301, Practice Quiz Generator.**

- [**Run in Browser**](...)
- [**Documentation**](...)
- [AGPLv3 License](LICENSE)
- Currently on hold; CAS and web client partially implemented.

**Problem Statement.**

- Display problems on:
    - Lambda Calculus,
    - Set Theory,
    - Predicate Logic,
    - Relations,
    - Functions,
    - and Finite Automata.
- Take user inputted answers.
- Display whether the inputted answer is correct.
- Display solution process if input is incorrect.
- Problems are generated on demand, and almost always unique.
- If an incorrect answer is provided, the question, or another question on the same topic, is presented later on.
- Display average performance per session.

**Problem Details.**

- Quiz Topics:
    - Lambda Calculus
        - Simplifying expressions
    - Set Theory
        - Simplifying expressions
        - Evaluating propositions
    - Predicate Logic
        - English to Symbolic and vice versa
        - Simplifying expressions
        - Negating expressions
        - Writing truth tables
        - Comparing propositions
    - Relations
        - Determining relation properties
    - Functions
        - Determining function properties
    - Finite Automata
        - Determining whether a string is accepted
        - Language from DFA
        - Language from NFA
        - Language to NFA
        - Language to DFA
        - Language to min DFA
        - NFA to min DFA
        - DFA to min DFA
    - Proofs
        - Equation parity proofs
        - Equation divisibility proofs
        - Set predicate proofs
        - Relation property proofs
        - Function property proofs
- Sessions
    - ~10 minutes
    - types
        - random (all topics)
        - set theory
        - predicate logic
        - relations
        - functions
        - finite automata
        - proofs
        - custom combo
- Problems
    - Mostly English instead of symbolic form.
- Input
    - yes/no/invalid
    - expression
    - list of properties
    - proof in pseudo-english 
- Feedback
    - Correct Answer
    - Difference from inputted answer
    - Solution process
- Statistics
    - average score for each topic
    - average time for each topic
    - plot with session scores
    - plot with session times
    - 10 hardest problems
    - 10 slowest problems

<!-- **Pseudo-English Problems.**
```
...
``` -->

**Pseudo-English.**


```
True.
```
```
30+x^2
```
```
forall x in RR. (P(x) -> Q(x))
```
```
For all real numbers, x, P(x) implies Q(x).
```
```
For all x in RR, if P(x) then Q(x).
```
```
Forall x in RR: Q(x) if P(x).
```
```
Forall x in RR: Q(x) when P(x).
```
```
Given P(0) and P(x) implies Q(x), we know that Q(0).
```
```
Given claim 1 and 5, we know that F(x).
```
- Structure
    1. Topic
    1. Paragraph
    1. Sentence
        - Signposts
            - Reference (recall [that] \_\_\_\_)
            - Inference (given [that] \_\_\_\_ and \_\_\_\_, we know [that] \_\_\_\_) (so, \_\_\_\_) (hence, \_\_\_\_) (thus, \_\_\_\_)
            - New topic (proposition. \_\_\_\_)
            - Implication (\_\_\_\_ implies \_\_\_\_)
            - Quantifier (forall \_\_\_\_, \_\_\_\_) (there exists \_\_\_\_) 
            - Restrictor (\_\_\_\_ where \_\_\_\_) (\_\_\_\_ such that \_\_\_\_)
        - Content
    1. Segment
    1. Component
    
```
S = ...
```

<!-- **Pseudo-English Proofs.**

```
S = intro ~ stmt+ ~ conclusion

intro = "Proposition. " ~ prop ~ "\n Proof. " ~ proof-type ~ "\n"

proof-type = "(Direct)"
           | "(Contrapositive)" 
           | "(Contradiction)" 
           | "(Inductive)"

conclusion = "So, " ~ prop ~ " []"


prop = quantifier ~ prop
     | 
``` -->

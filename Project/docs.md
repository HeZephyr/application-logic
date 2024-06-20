## Problem

You're cordially invited to develop a reasoner for EL or its extensions, then apply the normalisation procedure to the TBox

$$
\begin{aligned}
\mathcal{T} = \left\lbrace
\begin{array}{ccc}
A & \sqsubseteq & B \sqcap \exists r.C, \\
& \\
C & \sqsubseteq & \exists s.D, \\
\exists r.\exists s.\top & \sqcap & B \sqsubseteq D
\end{array}
\right\rbrace
\end{aligned}
$$

and then check whether it entails

$$
A\sqsubseteq D.
$$

The reasoner can be developed in any programming language you like, but comments or documentation are always necessary.

## Report: Development and Application of a Lightweight EL Reasoner for Terminology Box Normalization and Query Entailment Analysis

### 1 Introduction

This report documents the development and application of a simple reasoner for Description Logic (DL), specifically tailored towards the lightweight subset known as the EL family, which allows existential quantification and conjunction operations. The primary objective is to demonstrate the implementation of a reasoner capable of normalizing a given Terminology Box (TBox) and checking entailment queries. The problem statement involves a TBox with specific subsumption axioms and a query to verify whether A ⊑ D is entailed.

### 2 Implementation Details

#### 2.1 System Architecture

The solution is implemented in Python, leveraging object-oriented programming to model fundamental DL constructs such as Concepts, Roles, Existentials, Conjunctions, Subsumptions, and a TBox. Each class encapsulates the necessary logic to represent DL expressions and perform basic operations like equality checks, hashing, and string representation for readability.This design choice bolsters readability, maintainability, and extensibility.

##### 2.1.1 Core Classes and Their Functions

- **Concept**: Serves as the fundamental unit representing named concepts within the DL framework.
- **Role**: Models semantic relationships or properties connecting concepts.
- **Existential**: Formalizes existential restrictions, signifying the existence of related objects satisfying a given concept.
- **Conjunction**: Facilitates the representation of logical conjunctions between two concepts.
- **Subsumption**: Codifies subclass relationships, crucial for expressing hierarchical structures.
- **TBox**: Constitutes a container for subsumption axioms, featuring methods for TBox normalization, closure computation, and entailment checks.

#### 2.2 Reasoning Procedures

##### 2.2.1 Normalization of TBox

Normalization constitutes a pivotal process in refining TBox axioms into a less convoluted form, thereby streamlining entailment assessments. The `normalize` function embedded within the TBox class disassembles composite axioms, particularly those entailing conjunctions, into simpler, more manageable components.

##### 2.2.2 Closure Computation

The `closure` method systematically generates the deductive closure of the normalized TBox. By iteratively deducing all implications inherent in the axioms and incorporating these into the closure set, it establishes a comprehensive repository of derivable knowledge. This process is vital for entailment verification, as inclusion of a query in the closure signifies entailment.

##### 2.2.3 Entailment Verification

The `entails` method systematically assesses whether a proposed query (subsumption) is logically inferred from the TBox. Following normalization and closure computation, it confirms the presence of the query within the closure set, affirming entailment.

#### 2.3 Demonstration and Findings

Given the provided TBox and query, the implemented reasoner successfully demonstrates the following steps:

1. Initializes the TBox with the given subsumption axioms.
  
2. Normalizes the TBox, decomposing complex axioms.
  
3. Computes the closure to generate all derivable consequences.
  
4. Determines that A ⊑ D is indeed entailed by the TBox, showcasing the reasoner's correctness.
  

### 3 Conclusion and Future Perspectives

This project showcases a basic yet functional implementation of a DL reasoner focusing on EL-like languages. It emphasizes the importance of normalization in simplifying reasoning tasks and highlights the process of entailment verification through closure computation. The code is well-documented to facilitate understanding and future extensions to more expressive DL fragments.*

Potential enhancements include incorporating more advanced DL features (e.g., number restrictions, disjunctions), optimizing the reasoning algorithms for scalability, and integrating with standardized OWL or RDF frameworks for practical applications in knowledge representation and semantic web technologies.

The attached code snippet serves as a proof-of-concept for the described functionality and can be further extended or modified to suit more complex requirements in DL-based systems.

### References

- Horrocks, I., Sattler, U., Tobies, S. (1999). Practical Reasoning for Expressive Description Logics. In: Ganzinger, H., McAllester, D., Voronkov, A. (eds) Logic for Programming and Automated Reasoning. LPAR 1999. Lecture Notes in Computer Science(), vol 1705. Springer, Berlin, Heidelberg. https://doi.org/10.1007/3-540-48242-3_11
  
- Baader F, Calvanese D, McGuinness DL, Nardi D, Patel-Schneider PF, eds. *The Description Logic Handbook: Theory, Implementation and Applications*. 2nd ed. Cambridge University Press; 2007.
  
- Cucala D T, Grau B C, Horrocks I. Consequence-based reasoning for description logics with disjunction, inverse roles, number restrictions, and nominals[J]. arXiv preprint arXiv:1805.01396, 2018.

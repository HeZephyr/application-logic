class Concept:
    """
    Represents a concept in Description Logic, identified by a unique name.
    """
    def __init__(self, name):
        self.name = name

    def __eq__(self, other):
        return isinstance(other, Concept) and self.name == other.name

    def __hash__(self):
        return hash(self.name)

    def __repr__(self):
        return self.name

class Role:
    """
    Represents a role (also known as a property or relationship) connecting concepts in Description Logic.
    """
    def __init__(self, name):
        self.name = name

    def __eq__(self, other):
        return isinstance(other, Role) and self.name == other.name

    def __hash__(self):
        return hash(self.name)

    def __repr__(self):
        return self.name

class Existential:
    """
    Represents an existential restriction in Description Logic, indicating that there exists an object
    related via a role that satisfies a certain concept.
    """
    def __init__(self, role, concept):
        self.role = role
        self.concept = concept

    def __eq__(self, other):
        return isinstance(other, Existential) and self.role == other.role and self.concept == other.concept

    def __hash__(self):
        return hash((self.role, self.concept))

    def __repr__(self):
        return f"∃{self.role}.{self.concept}"

class Conjunction:
    """
    Represents the conjunction (AND operation) of two concepts.
    """
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __eq__(self, other):
        return isinstance(other, Conjunction) and self.left == other.left and self.right == other.right

    def __hash__(self):
        return hash((self.left, self.right))

    def __repr__(self):
        return f"({self.left} ⊓ {self.right})"

class Subsumption:
    """
    Represents a subsumption (subclass) relationship between two concepts.
    """
    def __init__(self, sub, sup):
        self.sub = sub
        self.sup = sup

    def __eq__(self, other):
        return isinstance(other, Subsumption) and self.sub == other.sub and self.sup == other.sup

    def __hash__(self):
        return hash((self.sub, self.sup))

    def __repr__(self):
        return f"{self.sub} ⊑ {self.sup}"

class TBox:
    """
    Represents a Terminology Box (TBox), containing a set of subsumption axioms defining the concept hierarchy.
    """
    def __init__(self, axioms=None):
        self.axioms = axioms if axioms else set()

    def add_axiom(self, axiom):
        self.axioms.add(axiom)

    def normalize(self):
        normalized_tbox = set()
        print("Normalizing TBox:")
        for axiom in self.axioms:
            print(f"Processing {axiom}")
            if isinstance(axiom.sub, Conjunction):
                # If the subsumption has a conjunction on the left side
                normalized_tbox.add(Subsumption(axiom.sub.left, axiom.sup))
                normalized_tbox.add(Subsumption(axiom.sub.right, axiom.sup))
                print(f"Added {Subsumption(axiom.sub.left, axiom.sup)}")
                print(f"Added {Subsumption(axiom.sub.right, axiom.sup)}")
            elif isinstance(axiom.sup, Conjunction):
                # If the subsumption has a conjunction on the right side
                normalized_tbox.add(Subsumption(axiom.sub, axiom.sup.left))
                normalized_tbox.add(Subsumption(axiom.sub, axiom.sup.right))
                print(f"Added {Subsumption(axiom.sub, axiom.sup.left)}")
                print(f"Added {Subsumption(axiom.sub, axiom.sup.right)}")
            else:
                # If the subsumption is a regular subsumption
                normalized_tbox.add(axiom)
                print(f"Added {axiom}")
        print("Normalization complete.\n")
        return TBox(normalized_tbox)

    def closure(self):
        closure_set = set(self.axioms)
        derivation = []
        added = True
        print("Computing closure:")
        while added:
            added = False
            new_axioms = set()
            for axiom1 in closure_set:
                for axiom2 in closure_set:
                    if isinstance(axiom1.sup, Concept) and axiom1.sup == axiom2.sub:
                        # If there is a match for subsumption and concept
                        new_axiom = Subsumption(axiom1.sub, axiom2.sup)
                        if new_axiom not in closure_set:
                            new_axioms.add(new_axiom)
                            derivation.append((axiom1, axiom2, new_axiom))
                            added = True
                            print(f"Derived {new_axiom} from {axiom1} and {axiom2}")
            closure_set.update(new_axioms)
        print("Closure computation complete.\n")
        return TBox(closure_set), derivation

    def entails(self, query):
        normalized_tbox = self.normalize()
        closure_tbox, derivation = normalized_tbox.closure()
        if query in closure_tbox.axioms:
            return True, derivation
        return False, derivation

    def __repr__(self):
        return "\n".join(str(axiom) for axiom in self.axioms)

# Utility function to print the derivation process
def print_derivation(derivation):
    print("\nDerivation Process:")
    for step in derivation:
        axiom1, axiom2, result = step
        print(f"From {axiom1} and {axiom2}, derive {result}")

# Define concepts
A = Concept("A")
B = Concept("B")
C = Concept("C")
D = Concept("D")
top = Concept("⊤")

# Define roles
r = Role("r")
s = Role("s")

# Define TBox
tbox = TBox({
    Subsumption(A, Conjunction(B, Existential(r, C))),
    Subsumption(C, Existential(s, D)),
    Subsumption(Conjunction(Existential(r, Existential(s, top)), B), D)
})

# Print TBox
print(f"TBox:\n{tbox}")

# Define the query
query = Subsumption(A, B)
print(f"\nQuery: {query}\n")

# Check entailment
entails, derivation = tbox.entails(query)

print(f"Entailment: {entails}")
if entails:
    print_derivation(derivation)
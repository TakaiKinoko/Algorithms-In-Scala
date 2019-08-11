#  18.335J/6.337J: Introduction to Numerical Methods

This is the repository of course materials for the 18.335J/6.337J course at MIT, taught by Prof. [Steven G. Johnson](http://math.mit.edu/~stevenj/), in Spring 2019.

Syllabus
--------

**Lectures**: Monday/Wednesday/Friday 3–4pm (2-190). **Office Hours:** Thursday 4–5pm (2-345).

**Topics**: Advanced introduction to numerical linear algebra and related numerical methods. Topics include direct and iterative methods for linear systems, eigenvalue decompositions and QR/SVD factorizations, stability and accuracy of numerical algorithms, the IEEE floating-point standard, sparse and structured matrices, and linear algebra software. Other topics may include memory hierarchies and the impact of caches on algorithms, nonlinear optimization, numerical integration, FFTs, and sensitivity analysis. Problem sets will involve use of [Julia](http://julialang.org/), a Matlab-like environment (little or no prior experience required; you will learn as you go).

**Prerequisites**: Understanding of linear algebra ([18.06](http://web.mit.edu/18.06/www/), [18.700](http://ocw.mit.edu/OcwWeb/Mathematics/18-700Fall-2005/CourseHome/), or equivalents). 18.335 is a graduate-level subject, however, so much more mathematical maturity, ability to deal with abstractions and proofs, and general exposure to mathematics is assumed than for 18.06!

**Textbook**: The primary textbook for the course is [_Numerical Linear Algebra_ by Trefethen and Bau](http://www.amazon.com/Numerical-Linear-Algebra-Lloyd-Trefethen/dp/0898713617). ([Readable online](http://owens.mit.edu:8888/sfx_local?bookid=9436&rft.genre=book&sid=Barton:Books24x7) with MIT certificates.)

**Other Reading**: Previous terms can be found in [branches of the 18335 git repository](https://github.com/mitmath/18335/branches). The [course notes from 18.335 in much earlier terms](https://ocw.mit.edu/courses/mathematics/18-335j-introduction-to-numerical-methods-fall-2010/) can be found on OpenCourseWare. For a review of iterative methods, the online books [Templates for the Solution of Linear Systems](http://www.netlib.org/linalg/html_templates/Templates.html) (Barrett et al.) and [Templates for the Solution of Algebraic Eigenvalue Problems](http://www.cs.utk.edu/~dongarra/etemplates/book.html) are useful surveys.

**Grading**: 33% problem sets (about six, ~biweekly). 33% **take-home mid-term exam** (posted Monday **Apr. 8** and due Tuesday **Apr. 9**), 34% **final project** ([one-page proposal](psets/proposal.md) due Friday March 22, project due Wednesday **May 15**).

* Psets will be [submitted electronically via Stellar](https://learning-modules.mit.edu/gradebook/index.html?uuid=/course/18/sp19/18.335).  Submit a good-quality PDF *scan* of any handwritten solutions and *also* a PDF *printout* of a Julia notebook of your computational solutions.

**TA:** [Sung Woo Jeong](http://math.mit.edu/directory/profile.php?pid=1782).

**Collaboration policy**: Talk to anyone you want to and read anything you want to, with three exceptions: First, you **may not refer to homework solutions from the previous terms** in which I taught 18.335. Second, make a solid effort to solve a problem on your own before discussing it with classmates or googling. Third, no matter whom you talk to or what you read, write up the solution on your own, without having their answer in front of you.

**Final Projects**: The final project will be a 8–15 page paper (single-column, single-spaced, ideally using the style template from the [_SIAM Journal on Numerical Analysis_](http://www.siam.org/journals/auth-info.php)), reviewing some interesting numerical algorithm not covered in the course. \[Since this is not a numerical PDE course, the algorithm should _not_ be an algorithm for turning PDEs into finite/discretized systems; however, your project _may_ take a PDE discretization as a given "black box" and look at some other aspect of the problem, e.g. iterative solvers.\] Your paper should be written for an audience of your peers in the class, and should include example numerical results (by you) from application to a realistic problem (small-scale is fine), discussion of accuracy and performance characteristics (both theoretical and experimental), and a fair comparison to at **least one competing algorithm** for the same problem. Like any review paper, you should _thoroughly reference_ the published literature (citing both original articles and authoritative reviews/books where appropriate \[rarely web pages\]), tracing the historical development of the ideas and giving the reader pointers on where to go for more information and related work and later refinements, with references cited throughout the text (enough to make it clear what references go with what results). (**Note:** you may re-use diagrams from other sources, but all such usage must be _explicitly credited_; not doing so is [plagiarism](http://writing.mit.edu/wcc/avoidingplagiarism).) Model your paper on academic review articles (e.g. read _SIAM Review_ and similar journals for examples).

A good final project will include:

* An extensive introduction and bibliography putting the algorithm in context.  Where did it come from, and what motivated its development?  Where is it commonly used (if anywhere)?  What are the main competing algorithms?  Were any variants of the algorithm proposed later?  What do other authors say about it?

* A clear description of the algorithm, with a summary of its derivation and key properties.   Don't copy long mathematical derivations or proofs from other sources, but do *summarize* the key ideas and results in the literature.

* A convincing validation on a representative/quasi-realistic test problem (i.e. show that your code works), along with an informative comparison to important competing algorithms.  For someone who is thinking about using the algorithm, you should strive to give them *useful* guidance on how the algorithm compares to competing algorithms: when/where should you consider using it (if ever)?   Almost never rely on actual timing results — see below!

Frequently asked questions about the final project:

1.  _Does it have to be about numerical linear algebra?_ No. It can be any numerical topic (basically, anything where you are computing a conceptually real result, not integer computations), excluding algorithms for discretizing PDEs.
2.  _Can I use a matrix from a discretized PDE?_ Yes. You can take a matrix from the PDE as input and then talk about iterative methods to solve it, etcetera. I just don't want the paper to be about the PDE discretization technique itself.
3.  _How formal is the proposal?_ Very informal—one page describing what you plan to do, with a couple of references that you are using as starting points. Basically, the proposal is just so that I can verify that what you are planning is reasonable and to give you some early feedback.
4.  _How much code do I need to write?_ A typical project (there may be exceptions) will include a working proof-of-concept implementation, e.g. in Julia or Python or Matlab, that you wrote to demonstrate that you understand how the algorithm works. Your code does _not_ have to be competitive with "serious" implementations, and I encourage you to download and try out existing "serious" implementations (where available) for any large-scale testing and comparisons.
5.  _How should I do performance comparisons?_ Be very cautious about timing measurements: unless you are measuring highly optimized code or only care about orders of magnitude, timing measurements are more about implementation quality than algorithms. Better to *measure something implementation-independent* (like flop counts, or matrix-vector multiplies for iterative algorithms, or function evaluations for integrators/optimizers), even though such measures have their own weaknesses.

* * *

Lecture Summaries and Handouts
------------------------------

### Lecture 1 (Feb 6)

* [pset 1](psets/pset1.pdf) and accompanying [notebook](https://nbviewer.jupyter.org/github/mitmath/18335/blob/master/psets/pset1.ipynb), due Friday Feb. 15.
* [Newton's method for square roots](notes/newton-sqrt.pdf) and accompanying [notebook](https://nbviewer.jupyter.org/github/mitmath/18335/blob/master/notes/Newton-Square-Roots.ipynb).

Brief overview of the huge field of numerical methods, and outline of the small portion that this course will cover. Key new concerns in numerical analysis, which don't appear in more abstract mathematics, are (i) performance (traditionally, arithmetic counts, but now memory access often dominates) and (ii) accuracy (both floating-point roundoff errors and also convergence of intrinsic approximations in the algorithms).

As a starting example, considered the convergence of Newton's method (as applied to square roots); see the handout and Julia notebook above.

**Further reading:** Googling "Newton's method" will find lots of references; as usual, the [Wikipedia article on Newton's method](https://en.wikipedia.org/wiki/Newton's_method) is a reasonable starting point. Beware that the terminology for the [convergence order](https://en.wikipedia.org/wiki/Rate_of_convergence) (linear, quadratic, etc.) is somewhat different in this context from the terminology for discretization schemes (first-order, second-order, etc.); see e.g. the linked Wikipedia article. Homer Reid's [notes on machine arithmetic](http://homerreid.dyndns.org/teaching/18.330/Notes/RootFinding.pdf) for [18.330](http://homerreid.dyndns.org/teaching/18.330/) are an excellent introduction that covers several applications and algorithms for root-finding. For numerical computation in 18.335, we will be using the Julia language: see this [information on Julia at MIT](https://github.com/mitmath/julia-mit).

### Julia tutorial (Feb 8: 5pm in 32-141) — optional

**Handout:** [Julia cheat-sheet](https://github.com/mitmath/julia-mit/blob/master/Julia-cheatsheet.pdf), [Julia intro slides](https://github.com/mitmath/julia-mit/blob/master/Julia-intro.pdf)

On Friday, 8 February, at 5pm in 32-141, I will give an (attendance-optional!) Julia tutorial, introducing the [Julia programming language and environment](http://julialang.org/) that we will use this term. Please see the [tutorial notes online](https://github.com/mitmath/julia-mit/blob/master/README.md).

Please **bring your laptops**, and try to install Julia and the IJulia interface first via the abovementioned tutorial notes. Several people will be at the tutorial session to help answer installation questions. Alternatively, you can use Julia online at [JuliaBox](https://juliabox.com/) without installing anything (although running things on your own machine is usually faster).

### Lecture 2 (Feb 8)

* [notes on floating-point](notes/lec8handout6pp.pdf) (18.335 Fall 2007; also [slides](notes/lec8.pdf))
* Julia [floating-point notebook](https://nbviewer.jupyter.org/github/mitmath/18335/blob/master/notes/Floating-Point-Intro.ipynb)
* some [floating-point myths](notes/fp-myths.pdf)

New topic: **Floating-point arithmetic**

The basic issue is that, for computer arithmetic to be fast, it has to be done in hardware, operating on numbers stored in a fixed, finite number of digits (bits). As a consequence, only a _finite subset_ of the real numbers can be represented, and the question becomes _which subset_ to store, how arithmetic on this subset is defined, and how to analyze the errors compared to theoretical exact arithmetic on real numbers.

In **floating-point** arithmetic, we store both an integer coefficient and an exponent in some base: essentially, scientific notation. This allows large dynamic range and fixed _relative_ accuracy: if fl(x) is the closest floating-point number to any real x, then |fl(x)-x| < ε|x| where ε is the _machine precision_. This makes error analysis much easier and makes algorithms mostly insensitive to overall scaling or units, but has the disadvantage that it requires specialized floating-point hardware to be fast. Nowadays, all general-purpose computers, and even many little computers like your cell phones, have floating-point units.

Went through some simple examples in Julia (see notebook above), illustrating basic syntax and a few interesting tidbits, in particular on the accuracy of summation algorithms, that we will investigate in more detail later.

Overview of **floating-point** representations, focusing on the IEEE 754 standard (see also handout from previous lecture). The key point is that the nearest floating-point number to _x_, denoted fl(_x_), has the property of _uniform relative precision_ (for |_x_| and 1/|_x_| < than some _range_, ≈10308 for double precision) that |fl(_x_)−_x_| ≤ εmachine|_x_|, where εmachine is the relative "machine precision" (about 10−16 for double precision). There are also a few special values: ±Inf (e.g. for [overflow](https://en.wikipedia.org/wiki/Arithmetic_overflow)), [NaN](https://en.wikipedia.org/wiki/NaN), and ±0 (e.g. for [underflow](https://en.wikipedia.org/wiki/Arithmetic_underflow)).

**Further reading:** [What Every Computer Scientist Should Know About Floating Point Arithmetic](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.22.6768) (David Goldberg, ACM 1991). William Kahan, [How Java's floating-point hurts everyone everywhere](http://www.cs.berkeley.edu/~wkahan/JAVAhurt.pdf) (2004): contains a nice discussion of floating-point myths and misconceptions. Trefethen, lecture 13. Homer Reid's [notes on machine arithmetic](http://homerreid.com/teaching/18.330/Notes/MachineArithmetic.pdf) for [18.330](http://homerreid.com/teaching/18.330/) are another nice introduction to this material.

### Lecture 3 (Feb 11)

* notes on the accuracy of [floating-point summation](notes/naivesum.pdf)
* [notes on backwards stability of summation](notes/summation-stability.pdf) — in class, I instead showed the same thing using the previous accuracy notes as a starting point, but similarly using the L₁ norm.

Analyzed the accumulated floating-point roundoff errors (see notes), explaining the results that we observed experimentally in the Julia notebook of the previous lecture.  In those experiments, all of the inputs were non-negative, so that the "condition number" factor in the derivation equalled 1.   In general, though, if you have *cancellations* between summands of opposite signs, the same analysis shows that the relative error of the output can be arbitrarily large.

**Stability:** Gave the obvious definition of **accuracy**, what we might call "forwards stability" = almost the right answer for the right input. Showed that this is often too strong; e.g. adding a sequence of numbers is not forwards stable. (Basically because the denominator in the relative forwards error, which is the exact sum, can be made arbitrarily small via cancellations.)

Define asymptotic notation O(ε): f(ε) is O(g(ε)) if there exist some constants C, ε₀ > 0 such that |f(ε)| < C|g(ε)| for all |ε|<ε₀. That is, g(ε) is an asymptotic _upper bound_ for f(ε) as ε goes to zero, ignoring constant factors C. (A similar notation is used in computational complexity theory, but in the limit of _large_ arguments n.) In the definitions of stability, we technically require [uniform convergence](https://en.wikipedia.org/wiki/Uniform_convergence): we must have O(ε) errors with the same constants C and ε₀ independent of the inputs x.  (The constants *can* depend on the dimension of x, however.)

More generally, we apply a weaker condition: "stability" = almost the right answer for almost the right input. (Gave the technical version of this, from the book.)

Often, it is sufficient to prove "backwards stability" = right answer for almost the right input. Showed that, in our example of adding a sequence of numbers, backwards stability seems to work where forwards stability failed. Then more rigorously proved that floating-point summation of _n_ numbers is backwards stable.

**Further reading**: See the further reading from the previous lecture. Trefethen, lectures 14, 15, and 3. See also the Wikipedia article on [asymptotic ("big O") notation](https://en.wikipedia.org/wiki/Big_O_notation); note that for expressions like O(ε) we are looking in the limit of _small_ arguments rather than of large arguments (as in complexity theory), but otherwise the ideas are the same.

### Lecture 4 (Feb 13)

* [notes on the equivalence of norms](notes/norm-equivalence.pdf)

When quantifying errors, a central concept is a norm, and we saw in our proof of backwards stability of summation that the choice of norm seems important. Defined norms (as in lecture 3 of Trefethen): for a vector space V, a norm takes any v∈V and gives you a real number ‖v‖ satisfying three properties:

* Positive definite: ‖v‖≥0, and =0 if and only if v=0
* Scaling: ‖αv‖ = |α|⋅‖v‖ for any scalar α.
* [Triangle inequality](https://en.wikipedia.org/wiki/Triangle_inequality): ‖v+w‖≤‖v‖+‖w‖

There are many norms, for many different vector spaces. Gave examples of norms of column vectors: _Lₚ_ norms (usually _p_ = 1, 2, or ∞) and weighted L₂ norms.  A complete (= continuous, essentially) normed vector space is called a [Banach space](https://en.wikipedia.org/wiki/Banach_space), and these error concepts generalize to f(x) when f and x are in any Banach spaces (including scalars, column vectors, matrices, …though infinite-dimensional Banach spaces are trickier).

Especially important in numerical analysis are functions where the inputs and/or outputs are matrices, and for these cases we need matrix norms. The most important matrix norms are those that are related to matrix operations. Started with the Frobenius norm. Related the Frobenius norm ‖A‖F to the square root of the sum of eigenvalues of A\*A, which are called the _singular values_ σ²; we will do much more on singular values later, but for now noted that they equal the squared eigenvalues of A if A\*\=A (Hermitian). Also defined the induced matrix norm, and bounded it below by the maximum eigenvalue magnitude of A (if A is square). For the L₂ induced norm, related it (without proof for now) to the maximum singular value.  A useful property of the induced norm is ‖AB‖≤‖A‖⋅‖B‖.  Multiplication by a unitary matrix Q (Q\* = Q⁻¹) preserves both the Frobenius and L₂ induced norms.

Equivalence of norms. Described fact that any two norms are equivalent up to a constant bound, and showed that this means that **stability in one norm implies stability in all norms.** Sketched proof (_only skimmed this_): (i) show we can reduce the problem to proving any norm is equivalent to e.g. L₁ on (ii) the unit circle; (iii) show any norm is continuous; and (ii) use a result from real analysis: a continuous function on a closed/bounded (compact) set achieves its maximum and minimum, the [extreme value theorem](http://en.wikipedia.org/wiki/Extreme_value_theorem). See notes handout.

**Further reading:** Trefethen, lecture 3. If you don't immediately recognize that A\*A has nonnegative real eigenvalues (it is positive semidefinite), now is a good time to review your linear algebra; see also Trefethen lecture 24.

### Lecture 5 (Feb 15)

* [pset 1 solutions](psets/pset1sol.pdf) and accompanying [notebook](https://nbviewer.jupyter.org/github/mitmath/18335/blob/master/psets/pset1sol.ipynb)
* [pset 2](psets/pset2.pdf), due at 3pm on Friday March 1.

Relate backwards error to forwards error, and backwards stability to forwards error (or "accuracy" as the book calls it). Show that, in the limit of high precision, the forwards error can be bounded by the backwards error multiplied by a quantity κ, the **relative condition number**. The nice thing about κ is that it involves only exact linear algebra and calculus, and is completely separate from considerations of floating-point roundoff. Showed that, for differentiable functions, κ can be written in terms of the induced norm of the Jacobian matrix.

Calculated condition number for square root, summation, and matrix-vector multiplication, as well as solving Ax=b, similar to the book. Defined the condition number of a matrix: for f(x)=Ax, the condition number is ‖A‖⋅‖x‖/‖Ax‖, which is bounded above by κ(A)=‖A‖⋅‖A⁻¹‖.

Related matrix _L_2 norm to eigenvalues of _B_\=_A_\*_A_. _B_ is obviously Hermitian (_B_\*\=_B_), and with a little more work showed that it is positive semidefinite: _x_\*_B__x_≥0 for any _x_. Reviewed and re-derived properties of eigenvalues and eigenvectors of Hermitian and positive-semidefinite matrices. Hermitian means that the eigenvalues are real, the eigenvectors are orthogonal (or can be chosen orthogonal). Also, a Hermitian matrix must be diagonalizable (I skipped the proof for this; we will prove it later in a more general setting). Positive semidefinite means that the eigenvalues are nonnegative.

Proved that, for a Hermitian matrix B, the **Rayleigh quotient** R(x)=x\*Bx/x\*x is bounded above and below by the largest and smallest eigenvalues of B (the "min–max theorem"). Hence showed that the L2 induced norm of A is the square root of the largest eigenvalue of _B_\=_A_\*_A_. Similarly, showed that the L₂ induced norm of A⁻¹, or more generally the supremum of ‖x‖/‖Ax‖, is equal to the square root of the inverse of the smallest eigenvalue of _A_\*._A_

Understanding norms and condition numbers of matrices therefore reduces to understanding the eigenvalues of _A_\*_A_ (or _A__A_\*). However, looking at it this way is unsatisfactory for several reasons. First, we would like to solve one eigenproblem, not two. Second, working with things like _A_\*_A_ explicitly is often bad numerically, because it squares the condition number \[showed that κ(_A_\*_A_)=κ(_A_)2\] and hence exacerbates roundoff errors. Third, we would really like to get some better understanding of _A_ itself. All of these concerns are addressed by the **singular value decomposition** or **SVD**, which we will derive next time.

**Further reading:** Trefethen, lectures 12, 14, 15, 24.  See any linear-algebra textbook for a review of eigenvalue problems, especially Hermitian/real-symmetric ones.  See also [these notes from 18.06](https://nbviewer.jupyter.org/github/stevengj/1806/blob/fall18/lectures/Conditioning.ipynb).

### Lecture 6 (Feb 19 — Tuesday is an "MIT Monday")

* [slides](notes/differential_equations.pdf)

Guest lecture by MIT Instructor Dr. [Chris Rackauckas](http://chrisrackauckas.com/): **Modern Differential Equations Solver Software: Where We Are and Where We're Headed**:

> In this talk we will discuss the current state of software in differential equations and see how the continued advances in computer science and numerical methods are likely to impact our software in the near future. Issues such as efficiency improvements for stiff and non-stiff differential equations will be addressed from a numerical analysis standpoint but backed with recent benchmarking results. Newer mathematical topics like random ordinary differential equations, jump diffusion equations, and adaptivity for stochastic differential equations will be introduced and the successes and limitations in current automatic software solutions will be discussed. We will close with a discussion on how recent computational advancements have been influencing the software implementations, specifically showing the effects of generic typing over abstract algorithms and implicit parallelism.

**Further reading:** See Chris's [survey of ODE software](http://www.stochasticlifestyle.com/comparison-differential-equation-solver-suites-matlab-r-julia-python-c-fortran/), his [DifferentialEquations.jl package](https://github.com/JuliaDiffEq/DifferentialEquations.jl) for Julia, and his [video introduction to DifferentialEquations.jl](http://www.stochasticlifestyle.com/intro-solving-differential-equations-julia/).

### Lecture 7 (Feb 20)

Guest lecture by Prof. Alan Edelman: the SVD, relationship to L₂ norms and condition numbers, applications (e.g. principal components analysis).

**Further reading:** Trefethen, lectures 4, 5.

### Lecture 8 (Feb 22)

* [Many viewpoints on linear regression notebook](https://github.com/alanedelman/18.337_2017/blob/master/lectures/Lecture04_0918%20RegressionManyWays/RegressionManyWays.ipynb).

Guest lecture by Prof. Alan Edelman: Generalized SVD (GSVD).  Least-square problems (via QR or SVD) and different viewpoints on linear regression: linear algebra, optimization, statistics, machine learning.

**Further reading:** Trefethen, lectures 7, 11.  [The GSVD: Where are the ellipses?](https://arxiv.org/abs/1901.00485).

### Lecture 9 (Feb 25)

Discussed solution of normal equations. Discussed condition number of solving normal equations directly, and noted that it squares the condition number—not a good idea if we can avoid it.

Introduced the alternative of QR factorization (finding an orthonormal basis for the column space of the matrix). Explained why, if we can do it accurately, this will give a good way to solve least-squares problems.

Gave the simple, but unstable, construction of the Gram-Schmidt algorithm, to find a QR factorization.  Analyzed its O(mn²) complexity (specifically, 2mn² flops), and commented that the "same" projection qqᵀa requires O(m²) operations if you perform it as (qqᵀ)a but O(m) operations if you perform it as q(qᵀa) — matrix operations are associative (but not commutative), but where you put the parentheses can make a big difference in performance!

Discussed loss of orthogonality in classical Gram-Schmidt, using a simple example, especially in the case where the matrix has nearly dependent columns to begin with.

**Further reading:** Trefethen, lectures 7, 8, 18, 19. Per Persson's [2006 18.335 Gram-Schmidt notes](notes/lec5.pdf) are also helpful, as is the [Wikipedia Gram-Schmidt article](https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process).

### Lecture 10 (Feb 27)

* [Gram-Schmidt IJulia notebook](https://nbviewer.jupyter.org/github/mitmath/18335/blob/master/notes/Gram-Schmidt.ipynb)
* Per Persson's [2006 18.335 Householder notes](notes/lec6.pdf) ([handout](notes/lec6handout6pp.pdf))

Discussed loss of orthogonality in classical Gram-Schmidt, using a simple example, especially in the case where the matrix has nearly dependent columns to begin with. Showed modified Gram-Schmidt and argued how it (mostly) fixes the problem. Numerical examples (see notebook).

Re-interpret Gram-Schmidt in matrix form as Q = AR1R2..., i.e. as multiplying A on the right by a sequence of upper-triangular matrices to get Q. The problem is that these matrices R may be very badly conditioned, leading to an inaccurate Q and loss of orthogonality.

Instead of multiplying A on the right by R's to get Q, however, we can instead multiply A on the left by Q's to get R. This leads us to the Householder QR algorithm.  Introduced Householder QR, emphasized the inherent stability properties of multiplying by a sequence of unitary matrices (as shown in pset 2). Show how we can convert a matrix to upper-triangular form (superficially similar to Gaussian elimination) via unitary Householder reflectors.

**Further reading:** Trefethen, lectures 7, 8, 16, 18, 19. It turns out that modified GS is backwards stable in the sense that the product QR is close to A, i.e. the function f(A) = Q\*R is backwards stable in MGS; this is why solving systems with Q,R (appropriately used as discussed in Trefethen lecture 19) is an accurate approximation to solving them with A. For a review of the literature on backwards-stability proofs of MGS, see e.g. [this 2006 paper by Paige et al.](https://epubs.siam.org/doi/10.1137/050630416) \[_SIAM J. Matrix Anal. Appl._ **28**, pp. 264-284\].

### Lecture 11 (Mar 1)

* performance experiments with matrix multiplication ([one-page](notes/matmuls-handout.pdf) or [full-size](notes/matmuls.pdf) versions)
* [ideal-cache terminology](notes/ideal-cache.pdf)
* [pset 2 solutions](psets/pset2sol.pdf)
* [pset 3](psets/pset3.pdf), due Friday March 15.

Finished Householder QR derivation, including the detail that one has a choice of Householder reflectors...we choose the sign to avoid taking differences of nearly-equal vectors. Gave flop count, showed that we don't need to explicitly compute Q if we store the Householder reflector vectors.

Counting arithmetic operation counts is no longer enough. Illustrate this with some performance experiments on a much simpler problem, matrix multiplication (see handouts). This leads us to analyze memory-access efficiency and caches and points the way to restructuring many algorithms.

Outline of the memory hierarchy: CPU, registers, L1/L2 cache, main memory, and presented simple 2-level ideal-cache model that we can analyze to get the basic ideas.

Analyzed cache complexity of simple row-column matrix multiply, showed that it asymptotically gets no benefit from the cache. Presented blocked algorithm, and showed that it achieves optimal Θ(n³/√Z) cache complexity.

Discussed some practical difficulties of the blocked matrix multiplication: algorithm depends on cache-size _Z_, and multi-level memories require multi-level blocking. Discussed how these ideas are applied to the design of modern linear-algebra libraries (LAPACK) by building them out of block operations (performed by an optimized BLAS).

**Further reading:** Wikipedia has a reasonable [introduction to memory locality](http://en.wikipedia.org/wiki/Locality_of_reference) that you might find useful. The optimized matrix multiplication shown on the handouts is called ATLAS, and you can find out more about it on the [ATLAS web page](http://math-atlas.sourceforge.net/). [Cache-oblivious algorithms](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.7911), describing ideal cache model and analysis for various algorithms, by Frigo, Leiserson, Prokop, and Ramachandran (1999). [Notes on the switch from LINPACK to LAPACK/BLAS in Matlab](https://www.mathworks.com/company/newsletters/articles/matlab-incorporates-lapack.html). The MIT course [6.172](http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-172-performance-engineering-of-software-systems-fall-2010/index.htm) has two lecture videos ([first](http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-172-performance-engineering-of-software-systems-fall-2010/video-lectures/lecture-8-cache-efficient-algorithms) and [second](http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-172-performance-engineering-of-software-systems-fall-2010/video-lectures/lecture-9-cache-efficient-algorithms-ii)) on cache-efficient algorithms, including a discussion of matrix multiplication.

### Lecture 12 (Mar 4)

* experiments with cache-oblivious matrix-multiplication ([handout](notes/oblivious-matmul-handout.pdf) or [full-size slides](notes/oblivious-matmul.pdf))
* [IJulia matrix-multiplication notebook](https://nbviewer.jupyter.org/github/mitmath/18335/blob/master/notes/Memory-and-Matrices.ipynb)

Introduced the concept of optimal cache-oblivious algorithms. Discussed cache-oblivious matrix multiplication in theory and in practice (see handout and Frigo et. al paper above).

Discussion of spatial locality and cache lines, with examples of dot products and matrix additions (both of which are "level 1 BLAS" operations with no temporal locality); you'll do more on this in pset 3.

**Further reading:** Frigo et al. paper from previous lecture. ATLAS web page above. See [Register Allocation in Kernel Generators](http://cscads.rice.edu/workshops/july2007/autotune-slides-07/Frigo.pdf) (talk by M. Frigo, 2007) on the difficulty of optimizing for the last level of cache (the registers) in matrix multiplication (compared to FFTs), and why a simple cache-oblivious algorithm is no longer enough. See e.g. the Wikipedia article on [row-major and column-major order](http://en.wikipedia.org/wiki/Row-major_order).

### Lecture 13 (Mar 6)

Review of **Gaussian elimination**. Reviewed the fact that this givs an A=LU factorization, and that we then solve Ax=b by solving Ly=b (doing the same steps to b that we did to A during elimination to get y) and then solving Ux=y (backsubstitution). Emphasized that you should **almost never compute A⁻¹** explicitly. It is just as cheap to keep L and U around, since triangular solves are essentially the same cost as a matrix-vector multiplication. Computing A⁻¹ is usually a mistake: you can't do anything with A⁻¹ that you couldn't do with L and U, and you are wasting both computations and accuracy in computing A⁻¹. A⁻¹ is useful in abstract manipulations, but whenever you see "x=A⁻¹b" you should interpret it for computational purposes as solving Ax=b by LU or some other method.

Introduced partial pivoting, and pointed out (omitting bookkeeping details) that this can be expressed as a PA=LU factorization where P is a permutation. Began to discuss backwards stability of LU, and mentioned example where U matrix grows exponentially fast with _m_ to point out that the backwards stability result is practically useless here, and that the (indisputable) practicality of Gaussian elimination is more a result of the types of matrices that arise in practice.

**Further reading:** Trefethen, lectures 20, 21, 22, 23.

### Lecture 14 (Mar 8)

Briefly discussed Cholesky factorization, which is Gaussian elimation for the special case of Hermitian positive-definite matrices, where we can save a factor of two in time and memory. More generally, if the matrix A has a special form, one can sometimes take advantage of this to have a more efficient Ax=b solver, for example: Hermitian positive-definite (Cholesky), tridiagonal or banded (linear-time solvers), lower/upper triangular (forward/backsubstitution), sparse (mostly zero—sparse-direct and iterative solvers, to be discussed later; typically only worthwhile when the matrix is much bigger than 1000×1000).

New topic: **eigenproblems**. Reviewed the usual formulation of eigenproblems and the characteristic polynomial, mentioned extensions to generalized eigenproblems and SVDs. Discussed diagonalization, defective matrices, and the generalization ot the Schur factorization.

Discussed diagonalization, defective matrices, and the generalization ot the Schur factorization. Proved (by induction) that every (square) matrix has a Schur factorization, and that for Hermitian matrices the Schur form is real and diagonal.

**Further reading:** Trefethen, lectures 20, 21, 22, 23. See all of the [special cases of LAPACK's linear-equation solvers](http://www.netlib.org/lapack/lug/node38.html).

### Lecture 15 (Mar 11)

* [notes on Hessenberg factorization](notes/lec14handout6pp.pdf)

Pointed out that an "LU-like" algorithm for eigenproblems, which computes the exact eigenvalues/eigenvectors (in exact arithmetic, neglecting roundoff) in a finite number of steps involving addition, subtraction, multiplication, division, and roots, is impossible. The reason is that no such algorithm exists (or can _ever_ exist) to find roots of polynomials with degree greater than 4, thanks to a theorem by Abel, Galois and others in the 19th century. Used the [companion matrix](http://en.wikipedia.org/wiki/Companion_matrix) to show that polynomial root finding is equivalent to the problem of finding eigenvalues. Mentioned the connection to other classic problems of antiquity (squaring the circle, trisecting an angle, doubling the cube), which were also proved impossible in the 19th century.

As a result, all eigenproblem methods must be _iterative_: they must consist of improving an initial guess, in successive steps, so that it converges towards the exact result to _any desired accuracy_, but never actually reaches the exact answer in general. A simple example of such a method is Newton's method, which can be applied to iteratively approximate a root of any nonlinear function to any desired accuracy, given a sufficiently good initial guess.

However, finding roots of the characteristic polynomial is generally a terrible way to find eigenvalues. Actually computing the characteristic polynomial coefficients and then finding the roots somehow (Newton's method?) is a disaster, incredibly ill-conditioned: gave the example of [Wilkinson's polynomial](http://en.wikipedia.org/wiki/Wilkinson%27s_polynomial). If we can compute the characteristic polynomial values implicitly somehow, directly from the determinant, then it is not too bad (if you are looking only for eigenvalues in some known interval, for example), but we haven't learned an efficient way to do that yet. The way LAPACK and Matlab actually compute eigenvalues, the QR method and its descendants, wasn't discovered until 1960.

The key to making most of the eigensolver algorithms efficient is reducing A to **Hessenberg form**: A=QHQ\* where H is upper triangular plus one nonzero value below each diagonal. Unlike Schur form, Hessenberg factorization _can_ be done exactly in a finite number \[Θ(m3)\] of steps (in exact arithmetic). H and A are similar: they have the same eigenvalues, and the eigenvector are related by Q. And once we reduce to Hessenberg form, all the subsequent operations we might want to do (determinants, LU or QR factorization, etcetera), will be fast. In the case of Hermitian A, showed that H is tridiagonal; in this case, most subsequent operations (even LU and QR factorization) will be Θ(m) (you will show this in HW)!

**Further reading:** Trefethen, lecture 24, 25. See [this Wilkinson polynomial Julia notebook](https://nbviewer.jupyter.org/github/mitmath/18335/blob/spring15/notes/Wilkinson-Polynomial.ipynb) for some experiments with polynomial roots in Julia, as well as [this more recent 18.06 notebook](https://nbviewer.jupyter.org/github/stevengj/1806/blob/fall18/lectures/Eigenvalue-Polynomials.ipynb).

### Lecture 16 (Mar 13)

Reviewed power method for biggest-|λ| eigenvector/eigenvalue and its the convergence rate.

Discussed how to use the power method to get multiple eigenvalues/vectors of Hermitian matrices by "deflation" (using orthogonality of eigenvectors). Discussed how, in principle, QR factorization of _An_ for large _n_ will give the eigenvectors and eigenvalues in descending order of magnitude, but how this is killed by roundoff errors.

Unshifted QR method: proved that repeatedly forming A=QR, then replacing A with RQ (as in pset 3) is equivalent to QR factorizing _An_. But since we do this while only multiplying repeatedly by unitary matrices, it is well conditioned and we get the eigenvalues accurately.

To make the QR method faster, we first reduce to Hessenberg form; you will show in pset 3 that this is especially fast when A is Hermitian and the Hessenberg form is tridiagonal. Second, we use shifts.  In particular, the worst case for the QR method, just as for the power method, is when eigenvalues are nearly equal. We can fix this by shifting.

Discussed inverse iteration and shifted-inverse iteration. Discussed Rayleigh-quotient iteration (shifted-inverse iteration with the Rayleigh-quotient eigenvalue estimate as the shift) and its convergence rate in the Hermitian case.  How, for Hermitian matrix the eigenvalue estimate has a much smaller error than the eigenvector (the error is squared) due to the fact that the eigenvalue is an extremum.

There are a number of additional tricks to further improve things, the most important of which is probably the Wilkinson shift: estimating μ from a little 2×2 problem from the last _two_ columns to avoid problems in cases e.g. where there are two equal and opposite eigenvalues. Some of these tricks (e.g. the Wilkinson shift) are described in the book, and some are only in specialized publications. If you want the eigenvectors as well as eigenvalues, it turns out to be more efficient to use a more recent "divide and conquer" algorithm, summarized in the book, but where the details are especially tricky and important. However, at this point I don't want to cover more gory details in 18.335. Although it is good to know the general structure of modern algorithms, especially the fact that they look nothing like the characteristic-polynomial algorithm you learn as an undergraduate, as a practical matter you are always just going to call LAPACK if the problem is small enough to solve directly. Matters are different for much larger problems, where the algorithms are not so bulletproof and one might need to get into the guts of the algorithms; this will lead us into the next topic of the course, iterative algorithms for large systems, in subsequent lectures.

Briefly discussed Golub–Kahn bidiagonalization method for SVD, just to get the general flavor. At this point, however, we are mostly through with details of dense linear-algebra techniques: the important thing is to grasp the fundamental ideas rather than zillions of little details, since in practice you're just going to use LAPACK anyway.

**Further reading:** See Trefethen, lectures 27–30, and Per Persson's [2006 notes](notes/lec15handout6pp.pdf) on power/inverse/Rayleigh iteration and on QR ([part 1](notes/lec15handout6pp.pdf) and [part 2](notes/lec16handout6pp.pdf)).

### Lecture 17 (Mar 15)

* [pset 3 solutions](psets/pset3sol.pdf)

Brief discussion of shifted QR method,

There are a number of additional tricks to further improve things, the most important of which is probably the Wilkinson shift: estimating μ from a little 2×2 problem from the last _two_ columns to avoid problems in cases e.g. where there are two equal and opposite eigenvalues. Some of these tricks (e.g. the Wilkinson shift) are described in the book, and some are only in specialized publications. If you want the eigenvectors as well as eigenvalues, it turns out to be more efficient to use a more recent "divide and conquer" algorithm, summarized in the book, but where the details are especially tricky and important. However, at this point I don't want to cover more gory details in 18.335. Although it is good to know the general structure of modern algorithms, especially the fact that they look nothing like the characteristic-polynomial algorithm you learn as an undergraduate, as a practical matter you are always just going to call LAPACK if the problem is small enough to solve directly. Matters are different for much larger problems, where the algorithms are not so bulletproof and one might need to get into the guts of the algorithms; this will lead us into the next topic of the course, iterative algorithms for large systems, in subsequent lectures.

Briefly discussed Golub–Kahn bidiagonalization method for SVD, just to get the general flavor. At this point, however, we are mostly through with details of dense linear-algebra techniques: the important thing is to grasp the fundamental ideas rather than zillions of little details, since in practice you're just going to use LAPACK anyway.

Start discussing (at a very general level) a new topic: **iterative algorithms**, usually for sparse matrices, and in general for matrices where you have a fast way to compute _Ax_ matrix-vector products but cannot (practically) mess around with the specific entries of _A_.

Emphasized that there are many iterative methods, and that there is no clear "winner" or single bulletproof library that you can use without much thought (unlike LAPACK for dense direct solvers). It is problem-dependent and often requires some trial and error. Then there is the whole topic of preconditioning, which we will discuss more later, which is even more problem-dependent. Briefly listed several common techniques for linear systems (Ax=b) and eigenproblems (Ax=λx or Ax=λBx).

**Further reading:** See Trefethen, lectures 27–30, and Per Persson's [2006 notes](notes/lec15handout6pp.pdf) on power/inverse/Rayleigh iteration and on QR ([part 1](notes/lec15handout6pp.pdf) and [part 2](notes/lec16handout6pp.pdf)).

### Lecture 18 (Mar 18)

Gave simple example of power method, which we already learned. This, however, only keeps the most recent vector Anv and throws away the previous ones. Introduced Krylov subspaces, and the idea of Krylov subspace methods: find the best solution in the whole subspace 𝒦ₙ spanned by {b,Ab,...,Aⁿ⁻¹b}.

Presented the **Arnoldi** algorithm. Unlike the book, I _start_ the derivation by viewing it as a modified Gram–Schmidt process, and prove that it is equivalent (in exact arithmetic) to GS on {b,b,Ab,A²b,...}, so it is an orthonormal basis for 𝒦ₙ.  Then we showed that this corresponds to partial Hessenberg factorization: AQₙ = QₙHₙ + h₍ₙ₊₁₎ₙqₙ₊₁eₙᵀ where Hₙ is upper-Hessenberg.

Discussed what it means to find the "best" solution in the Krylov subspace 𝒦ₙ. Discussed the general principle of Rayleigh–Ritz methods for approximately solving the eigenproblem in a subspace: finding the Ritz vectors/values (= eigenvector/value approximations) with a residual perpendicular to the subspace (a special case of a Galerkin method).

For Hermitian matrices A, I showed that the max/min Ritz values are the maximum/minimum of the Rayleigh quotient in the subspace, via the min–max theorem.   In fact, in this case Hₙ is Hermitian as well, so Hₙ is tridiagonal and most of the dot products in the Arnoldi process are zero.  Hence Arnoldi reduces to a three-term recurrence, and the Ritz matrix is tridiagonal.  This is called the **Lanczos** algorithm.

Noted that n steps of Arnoldi requires Θ(mn²) operations and Θ(mn) storage. If we only care about the eigenvalues and not the eigenvectors, Lanczos requires Θ(mn) operations and Θ(m+n) storage. However, this is complicated by rounding problems that we will discuss in the next lecture.

**Further reading:** Trefethen, lecture 31, 32, 33, 34. The online books, [Templates for the Solution of Linear Systems](http://www.netlib.org/linalg/html_templates/Templates.html) (Barrett et al.) and [Templates for the Solution of Algebraic Eigenvalue Problems](http://www.cs.utk.edu/~dongarra/etemplates/book.html), are useful surveys of iterative methods.

### Lecture 19 (Mar 20)

* [Arnoldi-iteration experiments](https://nbviewer.jupyter.org/github/mitmath/18335/blob/master/notes/Arnoldi.ipynb)
* [notes on restarting Arnoldi](notes/restarting-arnoldi.pdf)

Showed some computational examples (notebook above) of Arnoldi convergence. Discussed how rounding problems cause a loss of orthogonality in Lanczos, leading to "ghost" eigenvalues where extremal eigenvalues re-appear. In Arnoldi, we explicitly store and orthogonalize against all qj vectors, but then another problem arises: this becomes more and more expensive as n increases.

A solution to the loss of orthogonality in Lanczos and the growing computational effort in Arnoldi is restarting schemes, where we go for n steps and then restart with the k "best" eigenvectors.   If we restart with k=1 *every* step, then we essentially have the power method, so while restarting makes the convergence worse the algorithm still converges, and converges significantly faster than the power method for k>1.

Explained why restarting properly is nontrivial for k>1: we need to restart in such a way that maintains the Arnoldi (or Lanczos) property AQₙ = QₙHₙ + rₙeₙᵀ where Hₙ is upper-Hessenberg, rₙ is orthogonal to Qₙ, and eₙᵀ is only nonzero in the last column.  In particular, showed that the "obvious" naive restarting algorithm using k Ritz vectors *almost* works, but messes up the eₙᵀ property.  See the notes.

**Further reading:** See the section on [implicitly restarted Lanczos](http://www.cs.utk.edu/~dongarra/etemplates/node117.html) in [Templates for the Solution of Algebraic Eigenvalue Problems](http://www.cs.utk.edu/~dongarra/etemplates/book.html).

### Lecture 20 (Mar 22)

* [pset 4](psets/pset4.pdf), due Friday April 5.

Introduced the **GMRES** algorithm: compute the basis Qₙ for 𝒦ₙ as in Arnoldi, but then minimize the residual ‖Ax-b‖₂ for x∈𝒦ₙ using this basis.  This yields a small (n+1)×n least-squares problem involving Hₙ.

Discussed the convergence rate of GMRES and Arnoldi in terms of **polynomial** approximations. Following the book closely, showed that the relative errors (the residual norm ‖Ax-νx‖ or ‖Ax-b‖) can be bounded by minimizing the value p(λ) of a polynomial p(z) evaluated at the eigenvalues, where p has degree _n_ after the _n_\-th iteration. In Arnoldi, the λⁿ coefficient of p(λ) is 1, whereas in GMRES the constant coefficient p(0)=1. (There is also a factor of the condition number of the eigenvector matrix in GMRES, so it is favorable for the eigenvectors to be near-orthogonal, i.e for the matrix to be near-[normal](http://en.wikipedia.org/wiki/Normal_matrix).)

Using this, we can see that the most favorable situation occurs when the eigenvalues are grouped into a small cluster, or perhaps a few small clusters, since we can then make p(λ) small with a low-degree polynomial that concentrates a few roots in each cluster. This meanst that Arnoldi/GMRES will achieve small errors in only a few iterations. Morever we can see that a _few_ outlying eigenvalues aren't a problem, since p(z) will quickly attain roots there and effectively eliminate those eigenvalues from the error—this quantifies the intuition that Krylov methods "improve the condition number" of the matrix as the iteration proceeds, which is why the condition-number bounds on the error are often pessimistic. Conversely, the worst case will be when the eigenvalues are all spread out uniformly in some sense. Following examples 35.1 and 35.2 in the book, you can actually have two matrices with very similar small condition numbers but very different GMRES convergence rates, if in one case the eigenvalues are clustered and in the other case the eigenvalues are spread out in a circle around the origin (i.e. with clustered magnitudes |λ| but different phase angles).

Contrasted convergence with Arnoldi/Lanczos. As in the book, showed that Arnoldi also minimizes a polynomial on the eigenvalues, except that in this case the coefficient of the _highest_ degree term is constrained to be 1. (We proceeded somewhat backwards from the book: the book started with polynomial minimization and derived the Rayleigh-Ritz eigenproblem, whereas we went in the reverse direction.) Showed that this set of polynomials is shift-invariant, which explains why (as we saw experimentally) Arnoldi convergence is identical for A and A+μI. This is _not_ true for GMRES, and hence GMRES convergence is not shift-invariant—this is not suprising, since solving Ax=b and (A+μI)x=b can be very different problems.

Like Arnoldi/Lanczos, if GMRES does not converge quickly we must generally **restart** it, usually with a subspace of dimension 1; restarting GMRES repeatedly after k steps is called **GMRES(k)**. Unfortunately, unlike Arnoldi for the largest |λ|, restarted GMRES is _not guaranteed to converge_. If it doesn't converge, we must do something to speed up convergence: preconditioning (next time).

In many practical cases, unfortunately, the eigenvalues of A are _not_ mostly clustered, so convergence of GMRES may be slow (and restarted GMRES may not converge at all). The solution to this problem is **preconditioning**: finding an easy-to-invert M such that M⁻¹A has clustered eigenvalues (and is not nearly defective).

**Further reading:** Trefethen, lectures 34, 35, 40. The convergence of GMRES for very non-normal matrices is a complicated subject; see e.g. [this paper on GMRES for defective matrices](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.48.1733) or [this paper surveying different convergence estimates](http://eprints.maths.ox.ac.uk/1290/). Regarding convergence problems with GMRES, see this 2002 presentation by Mark Embree on [Restarted GMRES dynamics](http://www.caam.rice.edu/~embree/householder.pdf). [Cullum (1996)](http://link.springer.com/article/10.1007%2FBF02127693) reviews the relationship between GMRES and a similar algorithm called FOM that is more Galerkin-like (analogous to Rayleigh–Ritz rather than least-squares).

### Lecture 21 (Apr 1)

* [Sparse and dense linear solvers notebook](https://nbviewer.jupyter.org/github/mitmath/18335/blob/master/notes/Dense-and-Sparse.ipynb) with some numerical examples.

Continued to discuss preconditioning: finding an M such that MA (left preconditioning) or AM (right preconditioning) has clustered eigenvalues (solving MAx=Mb or AMy=b with x=My, respectively). Essentially, one can think of M as a crude approximation for A–1, or rather the inverse of a crude approximation of A that is easy to invert. Brief summary of some preconditioning ideas: multigrid, incomplete LU/Cholesky, Jacobi/block-Jacobi. (Since Jacobi preconditioners only have short-range interactions, they tend not to work well for matrices that come from finite-difference/finite-element discretizations on grids, but they can work well for diagonally dominant matrices that arise in spectral and integral-equation methods.)

**Conjugate-gradient (CG) methods:**

Began discussing gradient-based iterative solvers for Ax=b linear systems, starting with the case where A is Hermitian positive-definite. Our goal is the conjugate-gradient method, but we start with a simpler technique. First, we cast this as a minimization problem for f(x)=x\*Ax-x\*b-b\*x. Then, we perform 1d line minimizations of f(x+αd) for some direction d. If we choose the directions d to be the steepest-descent directions b-Ax, this gives the steepest-descent method. Discussed successive line minimization of f(x), and in particular the steepest-descent choice of d=b-Ax at each step. Explained how this leads to "zig-zag" convergence by a simple two-dimensional example, and in fact the number of steps is proportional to κ(A). We want to improve this by deriving a Krylov-subspace method that minimizes f(x) over _all_ previous search directions simultaneously.

Derived the conjugate-gradient method, by explicitly requiring that the n-th step minimize over the whole Krylov subspace (the span of the previous search directions). This gives rise to an orthogonality ("conjugacy", orthogonality through A) condition on the search directions, and can be enforced with Gram-Schmidt on the gradient directions. Then we will show that a Lanczos-like miracle occurs: most of the Gram-Schmidt inner products are zero, and we only need to keep the previous search direction.

**Further reading:**  [Templates for the Solution of Linear Systems](http://www.netlib.org/linalg/html_templates/Templates.html), chapter on [preconditioners](http://www.netlib.org/linalg/html_templates/node51.html), and e.g. _[Matrix Preconditioning Techniques and Applications](http://books.google.com?id=d9UdanCqJ1QC)_ by Ke Chen (Cambridge Univ. Press, 2005).   On MINRES and SYMMLQ: [Differences in the effects of rounding errors in Krylov solvers for symmetric indefinite linear systems](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.31.3064) by Sleijpen (2000); see also van der Vorst notes from Lecture 22 and the _Templates_ book. Trefethen, lecture 38 on CG. See also the useful notes, [An introduction to the conjugate gradient method without the agonizing pain](http://www.cs.cmu.edu/~quake-papers/painless-conjugate-gradient.pdf) by J. R. Shewchuk.

### Lecture 22 (Apr 3)

Finished derivation of conjugate gradient, by showing that it reduces to a three-term recurrence.

Discussed convergence of conjugate gradient: useless results like "exact" convergence in m steps (not including roundoff), pessimistic bounds saying that the number of steps goes like the square root of the condition number, and the possibility of superlinear convergence for clustered eigenvalues.

### Lecture 24 (Apr 5)

* [pset 4 solutions](psets/pset4sol.pdf)

Derived the preconditioned conjugate gradient method (showing how the apparent non-Hermitian-ness of MA is not actually a problem as long as M is Hermitian positive-definite). Mentioned the connection to approximate Newton methods (which is easy to see if we consider preconditioned steepest-descent with M approximately A⁻¹).

As an alternative to GMRES for non-Hermitian problems, considered the biCG algorithm. Derived it as in the van der Vorst notes below: as PCG on the Hermitian-indefinite matrix B=\[0,A;A\*,0\] with the "preconditioner" \[0,I;I,0\] (in the unpreconditioned case). Because this is Hermitian, there is still a conjugacy condition and it is still a Krylov method. Because it is indefinite, we are finding a saddle point and not a minimum of a quadratic, and _breakdown_ can occur if one of the denominators (e.g. d\*Bd) becomes zero. (This is the same as algorithm 39.1 in Trefethen, but derived very differently.) Because of this, you should almost never use the plain biCG algorithm. However, the biCG idea was the starting point for several "stabilized" refinements, culminating in biCGSTAB(L) that try to avoid breakdown by combining biCG with elements of GMRES. Another algorithm worth trying is the QMR algorithm.

Concluded with some rules of thumb about which type of solvers to use: LAPACK for small matrices (< 1000s×1000s), sparse-direct for intermediate-size sparse cases, and iterative methods for the largest problems or problems with a fast matrix⋅vector but no sparsity. One important point that we will discuss next time is that sparse-direct algorithms scale much better for sparse matrices that come from discretization of 2d PDEs than 3d PDEs. In general, some experimentation is required to find the best technique for a given problem, so software like Matlab or the Petsc library is extremely helpful in providing a quick way to explore many algorithms.

**Further reading:** [Templates for the Solution of Linear Systems](http://www.netlib.org/linalg/html_templates/Templates.html). A very nice overview of iterative methods for non-Hermitian problems can be found in these 2002 [Lecture Notes on Iterative Methods](http://www.math.uu.nl/people/vorst/lecture.html) by Henk van der Vorst (second section, starting with GMRES).

### Lecture 25 (Apr 8)

* [notes on sparse-direct solvers](notes/lec21handout6pp.pdf) from Fall 2006
* [IJulia notebook on sparse-direct solvers](https://nbviewer.jupyter.org/github/mitmath/18335/blob/master/notes/Nested-Dissection.ipynb)

**Sparse-direct solvers:** For many problems, there is an intermediate between the dense Θ(m3) solvers of LAPACK and iterative algorithms: for a sparse matrix A, we can sometimes perform an LU or Cholesky factorization while maintaining sparsity, storing and computing only nonzero entries for vast savings in storage and work. In particular, did a Matlab demo, a few experiments with a simple test case: the "discrete Laplacian" center-difference matrix on uniform grids that we've played with previously in 18.335. In 1d, this matrix is tridiagonal and LU/Cholesky factorization produces a bidiagonal matrix: Θ(m) storage and work! For a 2d grid, there are 4 off-diagonal elements, and showed how elimination introduces Θ(√m) nonzero entries in each column, or Θ(m3/2) nonzero entries in total. This is still not too bad, but we can do better. First, showed that this "fill-in" of the sparsity depends strongly on the ordering of the degrees of freedom: as an experiment, tried a _random_ reordering, and found that we obtain Θ(m2) nonzero entries (~10% nonzero). Alternatively, one can find re-orderings that greatly reduce the fill-in. One key observation is that the fill-in only depends on the pattern of the matrix, which can be interpreted as a [graph](http://en.wikipedia.org/wiki/Graph_%28mathematics%29): m vertices, and edges for the nonzero entries of A (an [adjacency matrix](http://en.wikipedia.org/wiki/Adjacency_matrix) of the graph), and sparse-direct algorithms are closely related to graph-theory problems. For our simple 2d Laplacian, the graph is just the picture of the grid connecting the points. One simple algorithm is the [nested dissection algorithm](http://en.wikipedia.org/wiki/Nested_dissection): recursively find a separator of the graph, then re-order the vertices to put the separator last. This reorders the matrix into a mostly block-diagonal form, with large blocks of zeros that are preserved by elimination, and if we do this recursively we greatly reduce the fill-in. Did a crude analysis of the the fill-in structure, resulting in the time/space complexity on the last page of the handoutw, for our 2d grid where separators are obvious; for more general matrices finding separators is a hard and important problem in graph theory.

**Further reading:** A [survey of nested-dissection algorithms](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.58.9722) (Khaira, 1992). List of [sparse-direct solver software](http://www.cs.utk.edu/~dongarra/etemplates/node388.html) by Dongarra and [another list by Davis](http://www.cise.ufl.edu/research/sparse/codes/). [Notes on multifrontal sparse-direct methods](http://www.cse.illinois.edu/courses/cs598mh/gupta.ps) by M. Gupta (UIUC, 1999). The book _[Direct Methods for Sparse Linear Systems](http://books.google.com/books?id=TvwiyF8vy3EC&lpg=PR1&ots=odauEC2c4k&dq=direct%20methods%20for%20sparse%20davis&pg=PR1#v=onepage&q&f=false)_ by Davis is a useful reference. Outside of Matlab, a popular library to help you solve spare problems is [PETSc](http://www.mcs.anl.gov/petsc/petsc-as/).

### Take-home Mid-term Exam (April 8)

To be posted on Monday April 8 by 5pm, due 5pm Tuesday April 9 (submitted electronically).   The exam is **open notes** and **open book** (including any material posted for the class: pset solutions and handouts).  **No other materials** may be used (**closed Internet**).

It will cover everything in 18.335 up to and including **lecture 20** and **pset 4**.

* my previous midterms: [fall 2008](https://github.com/mitmath/18335/blob/fall08/midterm.pdf) and [solutions](https://github.com/mitmath/18335/blob/fall08/midterm-sol.pdf), [fall 2009](https://github.com/mitmath/18335/blob/fall09/midterm-f09.pdf) (no solutions), [fall 2010](https://github.com/mitmath/18335/blob/fall10/midterm-f10.pdf) and [solutions](https://github.com/mitmath/18335/blob/fall10/midterm-sol-f10.pdf), [fall 2011](https://github.com/mitmath/18335/blob/fall11/midterm-f11.pdf) and [solutions](https://github.com/mitmath/18335/blob/fall11/midtermsol-f11.pdf), [fall 2012](https://github.com/mitmath/18335/blob/fall12/midterm-f12.pdf) and [solutions](https://github.com/mitmath/18335/blob/fall12/midtermsol-f12.pdf), [fall 2013](https://github.com/mitmath/18335/blob/fall13/midterm-f13.pdf) and [solutions](https://github.com/mitmath/18335/blob/fall13/midtermsol-f13.pdf), [spring 2015](https://github.com/mitmath/18335/blob/spring15/exams/midterm-s15.pdf) and [solutions](https://github.com/mitmath/18335/blob/spring15/exams/midtermsol-s15.pdf).

* [midterm exam](psets/midterm.pdf)
* [midterm solutions](psets/midtermsol.pdf)

### Lecture 26 (Apr 10)

* [overview of optimization](notes/optimization.pdf)

Several of the iterative algorithms so far have worked, conceptually at least, by turning the original linear-algebra problem into a minimization problem. It is natural to ask, then, whether we can use similar ideas to solve more general **optimization problems**, which will be the next major topic in 18.335.

Broad overview of optimization problems (see handout). The most general formulation is actually quite difficult to solve, so most algorithms (especially the most efficient algorithms) solve various special cases, and it is important to know what the key factors are that distinguish a particular problem. There is also something of an art to the problem formulation itself, e.g. a nondifferentiable minimax problem can be reformulated as a nicer differentiable problem with differentiable constraints.

CG easily generalizes to the [nonlinear conjugate-gradient algorithm](https://en.wikipedia.org/wiki/Nonlinear_conjugate_gradient_method) to (locally) minimize an *arbitrary* twice-differentiable f(x): the only changes are that r=-∇f is not simply b-Ax  and that the successive line minimizations min f(x+αd) need to be done numerically (an “easy” 1d optimization problem). The key point being that, near a local minimum of a smooth function, the objective is typically roughly quadratic (via Taylor expansion), and when that happens CG greatly accelerates convergence. (Mentioned Polak–Ribiere heuristic to help "reset" the search direction to the gradient if we are far from the minimum and convergence has stalled; see the Hager survey below for many more.)

Outlined application of nonlinear CG to Hermitian eigenproblems by minimizing the Rayleigh quotient (this is convex, and furthermore we can use the Ritz vectors to shortcut both the conjugacy and the line minimization steps). The generalization of this is the [LOBPCG](http://en.wikipedia.org/wiki/LOBPCG) algorithm.

**Further reading:** There are many textbooks on [nonlinear optimization](http://www.athenasc.com/nonlinbook.html) algorithms of various sorts, including specialized books on [convex optimization](http://web.stanford.edu/~boyd/cvxbook/), [derivative-free optimization](http://bookstore.siam.org/mp08/), etcetera.  A useful review of topology-optimization methods can be found in [Sigmund and Maute (2013)](https://link.springer.com/article/10.1007/s00158-013-0978-6). There are many variants of nonlinear conjugate-gradient, mainly to avoid bad behavior far from the minimum, as surveyed by Hager and Zhang, “[A Survey of Nonlinear Conjugate Gradient Methods](http://people.cs.vt.edu/~asandu/Public/Qual2011/Optim/Hager_2006_CG-survey.pdf),” *Pacific J. Optim.* 2, pp. 35-58 (2006).

### Lecture 27 (Apr 12)

* [notes on adjoint methods](notes/adjoint/adjoint.pdf) to compute gradients
* [notes on adjoint methods for recurrence relations](notes/adjoint/recurrence2.pdf)
* [adjoint example notebook](https://nbviewer.jupyter.org/github/mitmath/18335/blob/master/notes/adjoint/Adjoint-method.ipynb)

Introduction to **adjoint** methods and the remarkable fact that one can compute the gradient of a complicated function with about the same number of additional operations as computing the function _once_.

**Further reading:** A variant of adjoint methods is the well-known [backpropagation algorithm](https://en.wikipedia.org/wiki/Backpropagation) for neural networks, which can often be thought of as a recurrence relation (one per network layer).  Computers can sometimes now compute derivatives automatically via [automatic differentiation (AD)](https://en.wikipedia.org/wiki/Automatic_differentiation), and adjoint methods correspond to "reverse mode" AD.  For the largest-scale computational problems, solved by very complicated programs (often combining multiple external libraries), AD is still a challenge, however, and hand implementation of adjoint methods is often still required.

### Lecture 28 (Apr 17)

Adjoint methods for eigenproblems and recurrence relations, following notes from previous lecture.

### Lecture 29 (Apr 19)

Discussed some general concepts in local optimization. **Global convergence** means convergence to a _local_ optimum from any _feasible_ starting point; explained why finding the feasible region from an _infeasible_ starting point is in general as hard as global optimization. A typical **trust region** approach is to _locally approximate_ the objective and constraint functions by some _simple functions_ that are easy to optimize, optimize them within some localized trust region around a current point **x** to obtain a candidate step **y**, and then either take the step (e.g. if **y** is an improvement) and/or update the approximations and trust region (e.g. if **y** was not an improvement or the approximation and exact functions differed greatly). There are many, many algorithms that follow this general outline, but they differ greatly in what approximations they use (e.g. linear, quadratic, ...), what trust region they use, and what methods they use to update the trust region and to evaluate candidate steps. Often, the approximate functions are _convex_ so that convex-optimization methods can be used to solve the _trust-region subproblems_.

Went over a particular example of a nonlinear optimization scheme, solving the full inequality-constrained nonlinear-programming problem: the CCSA algorithms, as refined by Svanberg (2002). This is a surprisingly simple algorithm (the [NLopt](http://ab-initio.mit.edu/nlopt) implementation is only 300 lines of C code), but is robust and provably convergent, and illustrates a number of important ideas in optimization: optimizing an approximation to update the parameters **x**, guarding the approximation with trust regions and penalty terms, and optimizing via the dual function (Lagrange multipliers). Like many optimization algorithms, the general ideas are very straightforward, but getting the details right can be delicate!

Outlined the inner/outer iteration structure of CCSA, and the interesting property that it produces a sequence of feasible iterates from a feasible starting point, which means that you can stop it early and still have a feasible solution (which is very useful for many applications where 99% of optimal is fine, but feasibility is essential).

The inner optimization problem involving the approximate gᵢ functions turns out to be *much* easier to solve because it is *convex* and *separable* (gᵢ = a sum of 1d convex functions of each coordinate xⱼ).  Convexity allows us to use the technique of **duality** to turn the problem into an equivalent "dual" optimization problem, and separability makes this dual problem trivial to formulate and solve.   Began discussing the ideas of *Lagrangians* and duality using the Boyd textbook; we will continue this in the next lecture.

**Further reading:** Pages 1–10 of [Svanberg (2002) paper on CCSA algorithms](
http://dx.doi.org/10.1137/S1052623499362822) — I used the "linear and separable quadratic approximation" functions gᵢ in section 5.1; as far as I can tell the other example gᵢ functions have no general advantages.

### Lecture 30 (Apr 22)

* [slides from Boyd, chapter 5](notes/boyd-ch5-slides.pdf)

Started by reviewing the basic idea of Lagrange multipliers to find an extremum of one function f₀(x) and one equality constraint h₁(x)=0. We instead find an extremum of L(x,ν₁)=f₀(x)+ν₁h₁(x) over x and the _Lagrange multiplier_ ν₁. The ν₁ partial derivative of L ensures h₁(x)=0, in which case L=f0 and the remaining derivatives extremize f0 along the constraint surface. Noted that ∇L=0 then enforces ∇f₀=0 in the direction parallel to the constraint, whereas perpendicular to the constraint ν₁ represents a "force" that prevents x from leaving the h₁(x)=0 constraint surface.

Generalized to the Lagrangian L(x,λ,ν) of the general optimization problem (the "primal" problem) with both inequality and equality constraints, following chapter 5 of the Boyd and Vandenberghe book (see below) (section 5.1.1).

Described the KKT conditions for a (local) optimum/extremum (Boyd, section 5.5.3). These are true in problems with strong duality, as pointed out by Boyd, but they are actually true in much more general conditions. For example, they hold under the "LICQ" condition in which the gradients of all the active constraints are linearly independents. Gave a simple graphical example to illustrate why violating LICQ requires a fairly weird optimum, at a cusp of two constraints.

**Further reading:** _[Convex Optimization](http://www.stanford.edu/~boyd/cvxbook/)_ by Boyd and Vandenberghe (free book online), chapter 5. There are many sources on [Lagrange multipliers](http://en.wikipedia.org/wiki/Lagrange_multipliers) (the special case of equality constraints) online that can be found by googling.

### Lecture 31 (Apr 24)

* [notes on BFGS and quasi-Newton methods](notes/BFGS.pdf)

Began discussing quasi-Newton methods in general, and the BFGS algorithm in particular, following the notes.

**Further reading:** Wikipedia's articles on [quasi-Newton methods](http://en.wikipedia.org/wiki/Quasi-Newton_methods) and the [BFGS method](http://en.wikipedia.org/wiki/BFGS_method) have some useful links and summaries. Helpful derivations of many of the properties of BFGS updates, and many references, can be found in [this 1980 technical report by Dennis and Schnabel](http://www.cs.colorado.edu/department/publications/reports/docs/CU-CS-185-80.pdf) and for a generalization in [this 1994 paper by O'Leary and Yeremin](http://www.cs.umd.edu/~oleary/reprints/j39.pdf), for example.

### Lecture 32 (Apr 26)

Finished BFGS notes from last lecture.

### Lecture 33 (Apr 29)

Introduced derivative-free optimization algorithms, for the common case where you don't have too many parameters (tens or hundreds) and computing the gradient is inconvenient (complicated programming, even if adjoint methods are theoretically applicable) or impossible (non-differentiable objectives). Started by discussing methods based on linear interpolation of simplices, such as the COBYLA algorithm of Powell.

Discussed derivative-free optimization based on quadratic approximation by symmetric Broyden updates (as in BOBYQA/NEWUOA algorithm of Powell, for example). Updating the Hessian turns into a quadratic programming (QP) problem, and discussed solution of QPs by construction of the dual, turning it into either an unconstrained QP (and hence a linear system) for equality constrained problems, or a bound-constrained QP for inequality-constrained QPs.

Briefly discussed global optimization (GO). For general objectives, there are no "good" GO algorithms; it is easy to devise an objective for which GO is arbitrarily hard (the problem is NP-hard in general). Many (but not all) GO algorithms have a proof (often trivial) that they converge to the global optimum asymptotically, but little can be said about how _fast_ they converge, and in general there is no way to know when the global optimum has been found—stopping criteria for global optimization tend to be "stop when the design is good enough" or "stop when you run out of patience". In high dimensions, GO is often hopeless unless you have a function that is very "nice" (has only a few local optima). There are a few categories of GO algorithms, depending on how they perform the _global_ and _local_ portions of their search.

* _Stochastic global & local search:_ Algorithms like genetic algorithms, simulated annealing, and so on, typically rely on randomized algorithms for both exploring the global search space and for refining local optimum. These algorithms are easy to implement, assume little or nothing about the objective, are easy to parallelize, and have evocative natural analogies. As a result, they perhaps have a disproportionate mind share: they should often be treated as **methods of last resort**, because they don't exploit any mathematical structure of the objective (e.g. smoothness or even continuity).
* _Stochastic global & deterministic local search:_ The typical algorithm in this category is a "multistart" algorithm: perform repeated local optimizations from randomized starting points. This sort of algorithm can take advantage of very efficient local optimizers, especially if you have a differentiable function and the gradient is available. On the other hand, effective use often requires some tweaking to choose the stopping criteria of the local search, and the algorithms will often search the same local optimum many times. One attempt to deal with the latter problem is the MLSL algorithm, which is a multistart algorithm with a "filter" to prevent repeated searches of the same optimum (the filtering rule for starting points is not perfect, but it provably leads to each optimum being searched a finite number of times asymptotically).
* _Deterministic global & local search:_ These are typically "branch-and-bound" algorithms that systematically divide up the search domain and search the whole thing. For a black-box objective, one must asymptotically search the entire domain, but heuristics can be used to identify which subregions to search first, and a typical algorithm of this sort is DIRECT. Given more knowledge of the objective function, one can ideally devise a method to compute a _lower bound_ of the objective in each subregion, and in this way subregions can be eliminated from the search if their lower bound is above the best value found so far. A typical tool to construct such bounds is Taylor arithmetic, e.g. via the COSY INFINITY software. A typical example of branch-and-bound GO software based on analytical lower bounds is the BARON program.

**Further reading:** The book [Introduction to Derivative-Free Optimization](http://books.google.com/books?id=7c7X6tlcaHEC&lpg=PP1&ots=ljSCrl3WuI&dq=derivative%20free%20optimization%20conn&pg=PP1#v=onepage&q&f=false) by Andrew Conn et al is a reasonable starting point. See also the [derivative-free algorithms in NLopt](https://nlopt.readthedocs.io/en/latest/NLopt_Algorithms/) and the references thereof.  Several more stochastic algorithms in Julia are implemented in the [BlackBoxOptim](https://github.com/robertfeldt/BlackBoxOptim.jl) package.

### Lecture 34 (May 1)

**Handout:** [Notes](http://math.mit.edu/~stevenj/trap-iap-2011.pdf) on error analysis of the trapezoidal rule and Clenshaw-Curtis quadrature in terms of Fourier cosine series, and a [quick review of cosine series](http://math.mit.edu/~stevenj/cosines.pdf).

New topic: **numerical integration** (numerical quadrature). Began by basic definition of the problem (in 1d) and differences from general ODE problems. Then gave trapezoidal quadrature rule, and simple argument why the error generally decreases with the square of the number of function evaluations.

Showed numerical experiment (see handout) demonstrating that sometimes the trapezoidal rule can do much better than this: it can even have exponential convergence with the number of points! To understand this at a deeper level, I analyze the problem using Fourier cosine series (see handout), and show that the error in the trapezoidal rule is directly related to the convergence rate of the Fourier series. Claimed that this convergence rate is related to the smoothness of the periodic extension of the function, and in fact an analytic periodic function has Fourier coefficients that vanish exponentially fast, and thus the trapezoidal rule converges exponentially in that case. Proved by integration by parts of the Fourier series. In fact, we find that only the _odd_\-order derivatives at the endpoints need to be periodic to get accelerated convergence.

### Lecture 35 (May 3)

Explained the idea of Clenshaw–Curtis quadrature as a change of variables + a cosine series to turn the integral of _any_ function into the integral of periodic functions. This way, functions only need to be analytic on the interior of the integration interval in order to get exponential convergence. (See Wikipedia handout.)

Also explained (as in the handout) how to precompute the weights in terms of a discrete cosine transform, rather than cosine-transforming the function values every time one needs an integral, via a simple transposition trick.

Discussed the importance of nested quadrature rules for _a posteriori_ error estimation and adaptive quadrature. Discussed p-adaptive vs. h-adaptive adaptive schemes.

**Further reading**: Lloyd N. Trefethen, "[Is Gauss quadrature better than Clenshaw-Curtis?](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.157.4174)," _SIAM Review_ **50** (1), 67-87 (2008). Pedro Gonnet, [A review of error estimation in adaptive quadrature](http://dl.acm.org/citation.cfm?id=2333117), _ACM Computing Surveys_ **44**, article 22 (2012).

### Lecture 35 (May 6)

Explained connection of Clenshaw-Curtis quadrature and cosine series to Chebyshev polynomials. This leads into the general topic of Chebyshev approximation, and how we can approximate any smooth function on a finite interval by a polynomial with exponential accuracy (in the degree of the polynomial) as long as we interpolate via Chebyshev points.

Using Chebyshev approximation, explained how lots of problems can be solved by first approximating a nasty function via a polynomial, at which point one can just use easy methods for polynomials. Showed examples of root finding, minimization, integration, and solving ODEs via the [ApproxFun](https://github.com/ApproxFun/ApproxFun.jl) package for Julia, which implements a modern version of these ideas (following in the tracks of the pioneering [chebfun](http://www.chebfun.org/) package for Matlab).

**Further reading**: [Chebyshev polynomials on Wikipedia](http://en.wikipedia.org/wiki/Chebyshev_polynomials), free book online [Chebyshev and Fourier Spectral Methods](http://www-personal.umich.edu/~jpboyd/BOOK_Spectral2000.html) by John P. Boyd, the [chebfun package](http://www2.maths.ox.ac.uk/chebfun/) for Matlab by Trefethen et al., and the lecture notes from [Numerical Complex Analysis by Sheehan Olver](http://www.maths.usyd.edu.au/u/olver/teaching/NCA/).

### Lecture 36 (May 8)

Discussed integration with weight functions: I = ∫w(x)f(x)dx ≈ Iₙ = ∑wᵢf(xᵢ).  Even if the "weight" w(x) is "nasty" (discontinuous, singular, highly oscillatory…), we can do some (possibly expensive) precomputations of wᵢ and xᵢ *once* and re-use them for integrating many "nice" (smooth) functions f(x) with only a few quadrature points.

One approach to this is weighted Clenshaw–Curtis quadrature: expand f(x) in Chebyshev polynomials, i.e. f(cosθ) in a cosine series, as before via a trapezoidal rule (DCT), and then compute the integrals ∫w(cosθ)cos(kθ)sin(θ)dθ in whatever way you can … possibly by a very slowly converging method, but you only need to compute these weight integrals *once* and can then re-use them for many different f(x).

An alternative approach (for real w ≥ 0, and > 0 almost everywhere) is [Gaussian quadrature](https://en.wikipedia.org/wiki/Gaussian_quadrature): fit f(x) to a polynomial of degree n–1 by evaluating at n points consisting of the roots of an orthogonal polynomial qₙ₊₁ of degree n, where the polynomials {q₁,q₂,…} are formed via Gram–Schmidt orthogonalization of the basis {1,x,x²,…} with the inner product (p,q)=∫w(x)p(x)q(x)dx.   There is a beautiful proof that this integrates polynomials exactly up to degree 2n–1, it takes w(x) into account analytically, and by a more complicated analysis it converges exponentially for analytic f.  Moreover, it turns out that finding {q₁,q₂,…} is equivalent to performing the Lanczos iteration with the real-symmetric linear operator x, leading to a three-term recurrence.  We already saw one such three-term recurrence for the Chebyshev polynomials Tₙ(x), corresponding to the weight w(x)=1/√(1-x²), and other weight functions give rise to other orthogonal polynomials with their own 3-term recurrences: w=1 gives [Legendre polynomials](https://en.wikipedia.org/wiki/Legendre_polynomials), but there are also [Gegenbauer polynomials](https://en.wikipedia.org/wiki/Gegenbauer_polynomials), [Laguerre polynomials](https://en.wikipedia.org/wiki/Laguerre_polynomials) for w=e⁻ˣ on 0…∞,  [Hermite polynomials](https://en.wikipedia.org/wiki/Hermite_polynomials) for w=exp(-x²) on -∞…+∞, and others.   Moreover, the Lanczos tridiagonal eigenvalues turn out to be precisely the roots of qₙ₊₁ that we want, while the eigenvectors turn out to give the weights, and for a long time this surprising(!) O(n²) "Golub–Welsch" method was the method of choice for finding Gaussian quadrature nodes and weights.

**Further reading**: [Evans & Webster (1999)](https://doi.org/10.1016/S0377-0427(99)00213-7) describe the weighted Clenshaw–Curtis approach to oscillatory integrals; this and other methods are reviewed in Sheehan [Olver's thesis (2008)](http://www.maths.usyd.edu.au/u/olver/papers/OlverThesis.pdf).  Trefethen & Bau, lecture 37, discusses Gaussian quadrature and its connection to the Lanczos method.   More recently, O(n) methods have been discovered to find the Gaussian quadrature points and weights; see the references in the Julia [FastGaussQuadrature](https://github.com/JuliaApproximation/FastGaussQuadrature.jl) package.  An influential recent comparison of Clenshaw–Curtis and Gaussian quadrature is: Lloyd N. Trefethen, "[Is Gauss quadrature better than Clenshaw-Curtis?](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.157.4174)," _SIAM Review_ **50** (1), 67-87 (2008).

### Lecture 37 (May 10)

Discussed the importance of nested quadrature rules for _a posteriori_ error estimation (Clenshaw–Curtis, Gauss–Kronrod, Gauss–Patterson) and adaptive quadrature. Discussed p-adaptive vs. h-adaptive adaptive schemes.  Multidimensional integration and the curse of dimensionality.

### Lecture 38 (May 13)

**Handout:** [notes on FFTs](notes/fft-iap3.pdf)

Introduced the [discrete Fourier transform (DFT)](https://en.wikipedia.org/wiki/Discrete_Fourier_transform).   Talked about its history (Gauss!), properties (unitarity, convolution theorem), aliasing, special case of the [type-1 discrete cosine transform (DCT)](https://en.wikipedia.org/wiki/Discrete_cosine_transform), and applications (Chebyshev and other spectral methods for integration, PDEs, etcetera; signal processing, [multiplying large numbers](https://en.wikipedia.org/wiki/Sch%C3%B6nhage%E2%80%93Strassen_algorithm)), etc.

A [fast Fourier transform (FFT)](https://en.wikipedia.org/wiki/Fast_Fourier_transform) is an O(N log N) algorithm to compute the DFT.  There are many such algorithms, the most famous of which is the Cooley–Tukey algorithm (1965, though there were many precursors dating back to Gauss himself).

### Lecture 39 (May 15)

**Handout:** [slides on FFTs and FFTW](notes/FFTW.pdf)

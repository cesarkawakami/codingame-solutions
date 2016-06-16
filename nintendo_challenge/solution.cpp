#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <map>
#include <string>
#include <vector>
#include <cstdlib>

#include <execinfo.h>
#include <unistd.h>
#include <signal.h>

using namespace std;

void handler(int sig) {
    void *array[10];
    size_t size;

    // get void*'s for all entries on the stack
    size = backtrace(array, 10);

    // print out all the frames to stderr
    fprintf(stderr, "Error: signal %d:\n", sig);
    backtrace_symbols_fd(array, size, STDERR_FILENO);
    exit(1);
}

// template<typename S, typename T> inline void simattr(S &a, T &b, const S c, const T d) {
//     a = c;
//     b = d;
// }

template<typename T, int n> struct ModAny {
    static T inv(T a) {
        T t = 0;
        T newt = 1;
        T r = n;
        T newr = a;
        while (newr != 0) {
            T quotient = r / newr;

            T n_t = newt;
            T n_newt = t - quotient * newt;
            t = n_t;
            newt = n_newt;

            T n_r = newr;
            T n_newr = r - quotient * newr;
            r = n_r;
            newr = n_newr;

            // simattr(t, newt, newt, t - quotient * newt);
            // simattr(r, newr, newr, r - quotient * newr);
        }
        if (r > 1) {
            throw "not invertible";
        }
        if (t < 0) {
            return t + n;
        }
        return t;
    }
    static T mul(T a, T b) {
        return (a * b) % n;
    }
    static T div(T a, T b) {
        return (a * inv(b)) % n;
    }
    static T add(T a, T b) {
        return (a + b) % n;
    }
    static T sub(T a, T b) {
        return (a + n - b) % n;
    }
};

template<int n> struct Mod : ModAny<int, n> {};

template <int Q> struct Poly {
    int deg;
    // char coeff[513];
    char coeff[2049];

    Poly(int c = 0) {
        set_to(c);
    }
    Poly(const Poly<Q> &p) {
        set_to(p);
    }

    void set_to(const Poly<Q> &p) {
        deg = p.deg;
        memcpy(coeff, p.coeff, deg + 1);
    }
    void set_to(int c = 0) {
        if (c == 0) {
            deg = -1;
        } else {
            deg = 0;
            coeff[0] = c;
        }
    }

    bool is_zero() const {
        return deg == -1;
    }
    bool is_one() const {
        return deg == 0 && coeff[0] == 1;
    }

    string to_str_bin() const {
        char buf[1000];
        char *p = buf;
        bool first = true;
        for (int i = 0; i <= deg; ++i) {
            if (i % 8 == 0) {
                if (first) {
                    first = false;
                } else {
                    *(p++) = ' ';
                }
            }
            *(p++) = coeff[i] + '0';
        }
        *p = 0;
        char *q = buf;
        --p;
        while (p > q) {
            swap(*p, *q);
            ++q;
            --p;
        }
        char buf2[1000];
        sprintf(buf2, "<deg: %d, coeff: %s>", deg, buf);
        return string(buf2);
    }

    void check() const {
        return;
        assert(-1 <= deg);
        assert(deg <= 512);
        for (int i = 0; i <= deg; ++i) {
            assert(0 <= coeff[i]);
            assert(coeff[i] < Q);
        }
        if (deg >= 0) {
            assert(coeff[deg] > 0);
        }
    }
};

template<int Q> bool operator==(const Poly<Q> &a, const Poly<Q> &b) {
    return a.deg == b.deg && memcmp(a.coeff, b.coeff, a.deg + 1) == 0;
}

template<int Q> bool operator<(const Poly<Q> &a, const Poly<Q> &b) {
    if (a.deg != b.deg) {
        return a.deg < b.deg;
    }

    for (int i = a.deg; i >= 0; --i) {
        if (a.coeff[i] != b.coeff[i]) {
            return a.coeff[i] < b.coeff[i];
        }
    }

    return false;
}

template<int Q> void add(const Poly<Q> &a, const Poly<Q> &b, Poly<Q> &res) {
    if (a.deg < b.deg) {
        add(b, a, res);
        return;
    }
    res.set_to(a);
    for (int i = 0; i <= b.deg; ++i) {
        res.coeff[i] = Mod<Q>::add(
            res.coeff[i],
            b.coeff[i]
        );
    }
    while (res.deg >= 0 && res.coeff[res.deg] == 0) {
        --res.deg;
    }
    res.check();
}

template<int Q> void div(const Poly<Q> &a, const Poly<Q> &b, Poly<Q> &q, Poly<Q> &r) {
    if (b.is_zero()) {
        throw "polynomial division by zero!";
    }

    q.set_to(0);
    memset(q.coeff, 0, sizeof(q.coeff));
    r.set_to(a);

    q.check();
    r.check();
    while (r.deg >= b.deg) {
        q.check();
        r.check();
        q.deg = max(q.deg, r.deg - b.deg);
        q.coeff[r.deg - b.deg] = Mod<Q>::div(r.coeff[r.deg], b.coeff[b.deg]);
        q.check();
        r.check();
        for (int i = 0; i <= b.deg; ++i) {
            r.coeff[r.deg - b.deg + i] = Mod<Q>::sub(
                r.coeff[r.deg - b.deg + i],
                Mod<Q>::mul(b.coeff[i], q.coeff[r.deg - b.deg])
            );
            q.check();
        }
        while (r.deg >= 0 && r.coeff[r.deg] == 0) {
            --r.deg;
            q.check();
        }
        r.check();
    }
    q.check();
    r.check();
}

template<int Q> void mul(const Poly<Q> &a, const Poly<Q> &b, Poly<Q> &res) {
    res.deg = a.deg + b.deg;
    memset(res.coeff, 0, res.deg + 1);
    for (int i = 0; i <= a.deg; ++i) {
        for (int j = 0; j <= b.deg; ++j) {
            res.coeff[i + j] = Mod<Q>::add(
                res.coeff[i + j],
                Mod<Q>::mul(a.coeff[i], b.coeff[j])
            );
        }
    }
    while (res.deg >= 0 && res.coeff[res.deg] == 0) {
        --res.deg;
    }
    res.check();
}

template<int Q> void gcd(const Poly<Q> &a, const Poly<Q> &b, Poly<Q> &res) {
    if (b.is_zero()) {
        res.set_to(a);
        return;
    }

    Poly<Q> q, r;
    div(a, b, q, r);
    gcd(b, r, res);
    res.check();
}

template<int Q> void derive(const Poly<Q> &p, Poly<Q> &pprime) {
    if (p.is_zero()) {
        pprime.set_to(0);
        return;
    }
    for (int i = p.deg; i > 0; --i) {
        pprime.coeff[i - 1] = Mod<Q>::mul(p.coeff[i], i);
    }
    pprime.deg = p.deg - 1;
    while (pprime.deg >= 0 && pprime.coeff[pprime.deg] == 0) {
        --pprime.deg;
    }
    pprime.check();
}

template<int Q> void squish(const Poly<Q> &p, Poly<Q> &res) {
    if (p.is_zero()) {
        res.set_to(p);
    }
    for (int i = 0; i <= p.deg; ++i) {
        if (i % Q != 0) {
            assert(p.coeff[i] == 0);
        } else {
            res.coeff[i / Q] = p.coeff[i];
        }
    }
    res.deg = p.deg / Q;
    res.check();
}

template<int Q> void random_poly(int n, Poly<Q> &a) {
    // random poly with degree < n (strict inequality!!!)
    a.deg = n - 1;
    for (int i = 0; i <= a.deg; ++i) {
        a.coeff[i] = rand() % Q;
    }
    while (a.deg >= 0 && a.coeff[a.deg] == 0) {
        --a.deg;
    }
    a.check();
}

template<int Q> void square_free(const Poly<Q> &f, map<Poly<Q>, int> &factor, int multipl = 1) {
    Poly<Q> tmp, g, c, w, y, z, new_c, new_f;
    int i = 1;

    if (multipl == 1) {
        factor.clear();
    }

    derive(f, g);
g.check();
    if (!g.is_zero()) {
        gcd(f, g, c);
c.check();
        div(f, c, w, tmp);
w.check(); tmp.check();
        assert(tmp.is_zero());

        while (!w.is_one()) {
            gcd(w, c, y);
y.check();
            div(w, y, z, tmp);
z.check(); tmp.check();

            factor[z] += i * multipl;

            ++i;
            w.set_to(y);

            div(c, y, new_c, tmp);
new_c.check(); tmp.check();
            c.set_to(new_c);
        }

        if (!c.is_one()) {
            squish(c, new_c);
            square_free(new_c, factor, multipl * Q);
            return;
        } else {
            return;
        }
    } else {
        squish(f, new_f);
        square_free(new_f, factor, multipl * Q);
        return;
    }
}

template<int Q> void distinct_degree(const Poly<Q> &f, vector<pair<Poly<Q>, int> > &factor) {
    int i = 1;
    Poly<Q> fs(f), g, xqimf, new_xqimf, tmp, xqimf_x, new_fs;
    xqimf.deg = 1;
    xqimf.coeff[0] = 0;
    xqimf.coeff[1] = 1;
    factor.clear();
    while (fs.deg >= 2 * i) {
        // xqimf = xqimf * xqimf % f;
        mul(xqimf, xqimf, new_xqimf);
        div(new_xqimf, f, tmp, xqimf);

        // g = gcd(fs, xqimf - "x")
        xqimf_x.set_to(xqimf);
        xqimf_x.deg = max(xqimf_x.deg, 1);
        xqimf_x.coeff[1] = Mod<Q>::sub(xqimf_x.coeff[1], 1);
        while (xqimf_x.deg >= 0 && xqimf_x.coeff[xqimf_x.deg] == 0) {
            --xqimf_x.deg;
        }
        gcd(fs, xqimf_x, g);

        if (!g.is_one()) {
            factor.push_back(make_pair(g, i));
            div(fs, g, new_fs, tmp);
            fs.set_to(new_fs);
        }
        ++i;
    }
    if (!fs.is_one()) {
        factor.push_back(make_pair(fs, fs.deg));
    }
    if (factor.size() == 0) {
        factor.push_back(make_pair(f, 1));
        return;
    } else {
        return;
    }
}

template<int Q> void equal_degree(const Poly<Q> &ff, int d, vector<Poly<Q> > &factors) {
    throw "not implemented!";
}

// strongly based on NTL
typedef Poly<2> P2;
void equal_degree_tm_2(const P2 &a, int d, const P2 &f, P2 &w) {
    P2 y(a), z(a), tmp, tmp2;
    for (int i = 1; i < d; ++i) {
        mul(z, z, tmp);
        div(tmp, f, tmp2, z);
        add(y, z, tmp);
        y.set_to(tmp);
    }
    w.set_to(y);
}
void equal_degree_split_2(const P2 &f, int d, P2 &f1, P2 &f2) {
    P2 a, g, tmp;
    int n = f.deg;
    do {
        random_poly(n, a);
        equal_degree_tm_2(a, d, f, g);
    } while (g.deg <= 0);
    gcd(f, g, f1);
    div(f, f1, f2, tmp);
}
void equal_degree_inner_2(const P2 &f, int d, vector<P2> &factors) {
    if (f.deg == d) {
        factors.push_back(f);
        return;
    }

    P2 f1, f2;
    equal_degree_split_2(f, d, f1, f2);
    equal_degree_inner_2(f1, d, factors);
    equal_degree_inner_2(f2, d, factors);
}
template<> void equal_degree(const P2 &ff, int d, vector<P2> &factors) {
    P2 f(ff);
    int n = f.deg;
    int r = n/d;

    factors.clear();

    if (r == 0) {
        return;
    }

    if (r == 1) {
        factors.push_back(f);
        return;
    }

    if (d == 1) {
        P2 factor1;
        factor1.deg = 1;
        factor1.coeff[0] = 0;
        factor1.coeff[1] = 1;
        P2 factor2(factor1);
        factor2.coeff[0] = 1;
        factors.push_back(factor1);
        factors.push_back(factor2);
        return;
    }

    equal_degree_inner_2(f, d, factors);
}

template<int Q> void complete_factor(const Poly<Q> &f, map<Poly<Q>, int> &factors) {
    throw "not implemented!";
}
template<> void complete_factor(const P2 &f, map<P2, int> &factors) {
    factors.clear();
    map<P2, int> sq_factors;
    square_free(f, sq_factors);
    // printf("Square free factorization of %s:\n", f.to_str_bin().c_str());
    // for (auto sqit : sq_factors) {
    //     printf("  mult: %d, factor: %s\n", sqit.second, sqit.first.to_str_bin().c_str());
    // }
    for (auto sqit : sq_factors) {
        P2 p1(sqit.first);
        int multiplier1 = sqit.second;
        vector<pair<P2, int> > dd_factors;
        distinct_degree(p1, dd_factors);
        // printf("Distinct degree factorization of %s:\n", p1.to_str_bin().c_str());
        // for (auto ddit : dd_factors) {
        //     printf("  d: %d, factor: %s\n", ddit.second, ddit.first.to_str_bin().c_str());
        // }
        for (auto ddit : dd_factors) {
            P2 p2(ddit.first);
            int d = ddit.second;
            vector<P2> ed_factors;
            equal_degree(p2, d, ed_factors);
            // printf("Equal degree factorization of (%s, %d):\n", p2.to_str_bin().c_str(), d);
            for (auto edit : ed_factors) {
                // printf("  factor: %s\n", edit.to_str_bin().c_str());
                factors[edit] += multiplier1;
            }
        }
    }
}

int S;
unsigned int b[16];

P2 cur_f;
vector<pair<P2, P2> > answers;
void go(const P2 &factorand, const vector<pair<P2, int> > &factors, int index) {
    if (index >= int(factors.size())) {
        P2 other, tmp;
        div(factorand, cur_f, other, tmp);
        assert(tmp.is_zero());
        if (other.deg >= S) {
            return;
        }
        answers.push_back(make_pair(cur_f, other));
        return;
    } else {
        P2 old_f(cur_f), tmp;
        for (int i = 0; i <= factors[index].second; ++i) {
            go(factorand, factors, index + 1);
            mul(cur_f, factors[index].first, tmp);
            if (tmp.deg >= S) {
                cur_f.set_to(old_f);
                break;
            }
            cur_f.set_to(tmp);
        }
        cur_f.set_to(old_f);
    }
}
void go(const P2 &factorand, const map<P2, int> &factors) {
    vector<pair<P2, int> > factors_vec;
    for (auto it : factors) {
        factors_vec.push_back(make_pair(it.first, it.second));
    }
    cur_f.set_to(1);
    go(factorand, factors_vec, 0);
}


/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
int main() {
    signal(SIGSEGV, handler);
    signal(SIGABRT, handler);

    scanf("%d", &S);
    for (int i = 0; i < S/16; ++i) {
        scanf("%x", &b[i]);
    }

    P2 result_poly;
    for (int i = 0; i < 2 * S; ++i) {
        result_poly.coeff[i] = (b[i/32] >> (i % 32)) & 1;
    }
    result_poly.deg = 2 * S - 1;
    while (result_poly.deg >= 0 && result_poly.coeff[result_poly.deg] == 0) {
        --result_poly.deg;
    }

    // printf("Input: %s\n", result_poly.to_str_bin().c_str());

    map<P2, int> all_factors;
    complete_factor(result_poly, all_factors);

    // printf("List of Factors:\n");
    // for (auto el : all_factors) {
    //     printf("  mult: %d, factor: %s\n", el.second, el.first.to_str_bin().c_str());
    // }

    go(result_poly, all_factors);

    typedef unsigned int uint;
    vector<string> final_answers;
    for (auto el : answers) {
        // printf("sol: (%s, %s)\n", el.first.to_str_bin().c_str(), el.second.to_str_bin().c_str());
        uint a[16] = {0};
        for (int i = 0; i <= el.first.deg; ++i) {
            a[i/32] |= (uint(el.first.coeff[i]) << (i % 32));
        }
        for (int i = 0; i <= el.second.deg; ++i) {
            a[(S + i) / 32] |= (uint(el.second.coeff[i]) << (i % 32));
        }
        char buf[4000];
        char *p = buf;
        const char *spc = "";
        for (int i = 0; i < S/16; ++i) {
            int written = sprintf(p, "%s%08x", spc, a[i]);
            p += written;
            spc = " ";
        }
        final_answers.push_back(string(buf));
    }
    sort(final_answers.begin(), final_answers.end());
    for (auto el : final_answers) {
        printf("%s\n", el.c_str());
    }

    return 0;
}

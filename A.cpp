#include <bits/stdc++.h>
#define fo(i, d, c) for (int i = d; i <= c; i++)
#define fod(i, c, d) for (int i = c; i >= d; i--)
#define maxn 100010
#define N 1010
#define fi first
#define se second
#define pb emplace_back
#define en cout << "\n";
#define int long long
#define inf (int)1e18
#define pii pair<int, int>
#define vii vector<pii>
#define lb(x) x & -x
#define bit(i, j) ((i >> j) & 1)
#define offbit(i, j) (i ^ (1LL << j))
#define onbit(i, j) (i | (1LL << j))
#define vi vector<int>
template <typename T1, typename T2>
bool minimize(T1 &a, T2 b)
{
    if (a > b)
    {
        a = b;
        return true;
    }
    return false;
}
template <typename T1, typename T2>
bool maximize(T1 &a, T2 b)
{
    if (a < b)
    {
        a = b;
        return true;
    }
    return false;
}
using namespace std;
const int nsqrt = 450;
const int mod = 1e9 + 7;
template <class T>
struct Fenwick_Tree
{
    vector<T> bit_add, bit_sub;
    int n;

    Fenwick_Tree(int n = 0) : n(n), bit_add(n + 1), bit_sub(n + 1) {}

    void clear()
    {
        fill(bit_add.begin(), bit_add.end(), T(0));
        fill(bit_sub.begin(), bit_sub.end(), T(0));
    }

    void update(int u, int v, T val)
    {
        for (int i = u; i <= n; i += i & -i)
        {
            bit_add[i] += val;
            bit_sub[i] += 1LL * (u - 1) * val;
        }
        for (int i = v; i <= n; i += i & -i)
        {
            bit_add[i] -= val;
            bit_sub[i] -= 1LL * v * val;
        }
    }

    void update(int u, T val) { update(u, u, val); }

    T get(int u)
    {
        T ans1 = 0, ans2 = 0;
        for (int i = u; i; i -= i & -i)
        {
            ans1 += bit_add[i];
            ans2 += bit_sub[i];
        }
        return u * ans1 - ans2;
    }

    T get(int l, int r) { return get(r) - get(l - 1); }
};
Fenwick_Tree<int> t(maxn);
int n;
vi ke[maxn];
int par[maxn][20];
int h[maxn];
int head[maxn], ind[maxn], pos[maxn], sz[maxn], tail[maxn];
int cnt, chaincnt;
void dfs(int u)
{
    sz[u] = 1;
    for (int v : ke[u])
    {
        if (v == par[u][0])
            continue;
        h[v] = h[u] + 1;
        par[v][0] = u;
        fo(i, 1, 19) par[v][i] = par[par[v][i - 1]][i - 1];
        dfs(v);
        sz[u] += sz[v];
    }
}
void hld(int u)
{
    if (head[chaincnt] == 0)
        head[chaincnt] = u;
    ind[u] = chaincnt;
    pos[u] = ++cnt;
    int maxx = 0, sc = -1;
    for (int v : ke[u])
    {
        if (v == par[u][0])
            continue;
        if (sz[v] > maxx)
        {
            maxx = sz[v], sc = v;
        }
    }
    if (sc != -1)
        hld(sc);
    for (int v : ke[u])
        if (v != sc && v != par[u][0])
        {
            ++chaincnt;
            hld(v);
        }
    tail[u] = cnt;
}
int lca(int u, int v)
{
    if (h[u] < h[v])
        swap(u, v);
    int del = h[u] - h[v];
    fod(i, 19, 0) if (bit(del, i)) u = par[u][i];
    if (u == v)
        return u;
    fod(i, 19, 0) if (par[u][i] != par[v][i]) u = par[u][i], v = par[v][i];
    return par[u][0];
}
int getup(int u, int v)
{
    int ans = 0;
    int uchain, vchain = ind[v];
    while (1)
    {
        uchain = ind[u];
        if (uchain == vchain)
        {
            ans += t.get(pos[v] + 1, pos[u]);
            return ans;
        }
        ans += t.get(pos[head[uchain]], pos[u]);
        u = par[head[uchain]][0];
    }
}
int query(int u, int v)
{
    int ans = 0;
    int _lca = lca(u, v);
    ans += getup(u, _lca);
    ans += getup(v, _lca);
    return ans;
}
int q;
array<int, 3> edge[maxn],EDGE[maxn];
int res[maxn];
vector<array<int, 4>> qry1;
main()
{
#define name "TASK"
    if (fopen(name ".inp", "r"))
    {
        freopen(name ".inp", "r", stdin);
        freopen(name ".out", "w", stdout);
    }
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);
    cin >> n >> q;
    fo(i, 1, n - 1)
    {
        int u, v, w;
        cin >> u >> v >> w;
        ke[u].pb(v);
        ke[v].pb(u);
        edge[i] = {w, u, v};
        EDGE[i] = edge[i];
    }
    dfs(1);
    hld(1);
    fo(i, 1, q)
    {
        int u, v, w;
        char t;
        cin >> t >> u >> v;
        if (t == 'P')
        {
            cin >> w;
            qry1.push_back({w, i, u, v});
        }
        else
            qry1.push_back({v, i, u, 0});
    }
    sort(qry1.begin(), qry1.end(), [](array<int, 4> a, array<int, 4> b)
         { return a[0] < b[0] or (a[0] == b[0] and a < b); });
    sort(edge + 1, edge + n);
    int now = 1;
    for (auto [w, id, u, v] : qry1)
    {
        while (edge[now][0] <= w and now < n)
        {
            auto [_, x, y] = edge[now++];
            t.update(max(pos[x], pos[y]), max(pos[x], pos[y]), 1);
        }
        if (v != 0)
            res[id] = query(u, v);
        else
        {
            auto [weight, x, y] = EDGE[u];
            if (par[y][0] == x)
                res[id] = t.get(pos[y] + 1, tail[y]);
            else
                res[id] = now - 1 - (weight <= w) - t.get(pos[x] + 1, tail[x]);
        }
    }
    fo(i, 1, q) cout << res[i] << "\n";
}

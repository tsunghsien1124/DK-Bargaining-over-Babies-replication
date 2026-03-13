#=
    bargaining.jl  —  Play with bargaining parameters only

    Bargaining parameters in this model:
      χ_m  (chi[m])  — male share of child-rearing costs (0 to 1)
      χ_f  (chi[f])  — female share = 1 - χ_m  (derived)
      limited_commitment — whether partners can "block" fertility

    Since these don't affect wages or preference draws, the script:
      1. Loads benchmark, solves full model ONCE
      2. For each experiment, only re-runs: cost setup → decisions → distributions → aggregation
         (skips wage discretization + preference generation ≈ 2× faster)

    Usage:  julia bargaining.jl
    Edit:   scroll to EXPERIMENTS section at the bottom

    INDEX CONVENTION (Fortran 0-based → Julia 1-based):
      gender:  f=0,m=1 → f=1,m=2
      educ:    al=0,hs=1,co=2 → al=1,hs=2,co=3
      parity:  0:NN-1 → 1:NN
      intent:  0=no,1=yes → 1=no,2=yes
      h(LFP):  0=home,1=work → 1=home,2=work
=#

using Distributions: Normal, cdf, quantile
using Roots: find_zero
using Printf
using Random

# ═══════════════════════════════════════════════════════════════
# CONSTANTS
# ═══════════════════════════════════════════════════════════════

const NN = 3; const TT = 8; const SS = 100_000; const WW = 6
const f = 1; const m = 2
const al = 1; const hs = 2; const co = 3

# ═══════════════════════════════════════════════════════════════
# GLOBAL STATE
# ═══════════════════════════════════════════════════════════════

# parameters
alpha = 0.0; beta_d = 0.0
mu = zeros(NN,2,2); sig = zeros(2); rho = 0.0; pi_d = 0.0
nu = zeros(NN+1,2,2,2)
chi = zeros(2); chi_base = 0.0
phi_c = 0.0; phi_u = 0.0
w_y = 0.0; w_y_base = 0.0
w_f_bar = zeros(2); dist_hc = zeros(2); p_cost = 0.0
sub_ = zeros(6, NN)
LFP_without_targ = zeros(2)
limited_commitment = true

# solutions
v_g = zeros(2,SS,NN,2)
w_f = zeros(WW,2); dist_w = zeros(WW,2)
i_g = zeros(Int,2,SS,NN+1,TT,WW,2)
eta_g = zeros(2,SS,NN+1)
h_g = zeros(Int,SS,NN+1,TT,WW,2)
X_t = zeros(2,SS,NN+1,TT+1,WW,2)
distrib = zeros(SS,NN+1,TT,WW,3)
tot_dist = zeros(NN+1,3)
t_phi_u = zeros(2,NN); t_phi_c = zeros(2,NN)
t_phi = zeros(2,2,NN,WW,2)
limited_ = zeros(2,NN,WW,2)
realizations = zeros(SS,2)

# aggregates
fert_int = zeros(2,2,NN+2,3)
fert_out = zeros(2,2,NN+2,3)
fert_int_cond = zeros(2,2,2,2,3)
fert_rat = zeros(3)
fert_age_first = zeros(3); fert_age_last = zeros(3)

# ═══════════════════════════════════════════════════════════════
# WAGE DISCRETIZATION (run once at startup)
# ═══════════════════════════════════════════════════════════════

function discretize_wages!(ie, pc, wy, wbar, lfp)
    d = Normal(0.0,1.0)
    σ = find_zero(σ_ -> exp(log(pc)-σ_*(quantile(d,1.0-lfp)-σ_/2))*(1-cdf(d,quantile(d,1-lfp)-σ_))/lfp - wbar, 1.0)
    μl = log(pc) - σ*quantile(d,1-lfp); μm = exp(μl+σ^2/2)
    th = [0.0, pc, pc+0.5wy, pc+0.75wy, pc+wy, pc+1.25wy]
    w_f[1,ie] = th[2]; dist_w[1,ie] = cdf(Normal(μl,σ), log(th[2]))
    for i in 2:5
        ls=(log(th[i+1])-μl-σ^2)/σ; hs_=(log(th[i])-μl-σ^2)/σ
        lb=(log(th[i+1])-μl)/σ;      hb=(log(th[i])-μl)/σ
        w_f[i,ie] = μm*(cdf(d,ls)-cdf(d,hs_))/(cdf(d,lb)-cdf(d,hb))
        dist_w[i,ie] = cdf(d,lb)-cdf(d,hb)
    end
    hs6=(log(th[6])-μl-σ^2)/σ; hb6=(log(th[6])-μl)/σ
    w_f[6,ie] = μm*(1-cdf(d,hs6))/(1-cdf(d,hb6)); dist_w[6,ie] = 1-cdf(d,hb6)
    w_f[:,ie] .-= pc
end

# ═══════════════════════════════════════════════════════════════
# PREFERENCE GENERATION (run once at startup)
# ═══════════════════════════════════════════════════════════════

function get_preferences!()
    global v_g
    l = max(0.0,-rho); r = min(1.0,1.0-rho); v_g .= 0.0
    for ie in 1:2, is in 1:SS
        uu = l+realizations[is,1]*(r-l); uv = 2realizations[is,2]
        ux1=min(uv,uu); ux2=min(uv-ux1,1-uu); ux3=min(uv-ux1-ux2,uu); ux4=min(uv-ux1-ux2-ux3,1-uu)
        U1=(ux1-ux3)+(ux2-ux4); U2=uu-(ux1-ux3)+(ux2-ux4)
        for n in 1:NN
            v_g[f,is,n,ie] = mu[n,f,ie] + (U1-0.5)*sqrt(12sig[f])
            v_g[m,is,n,ie] = mu[n,m,ie] + (U2-0.5)*sqrt(12sig[m])
        end
    end
end

# ═══════════════════════════════════════════════════════════════
# COST SETUP (re-run when χ changes)
# ═══════════════════════════════════════════════════════════════

function setup_costs!()
    global t_phi_u, t_phi_c, t_phi, limited_
    chi[f] = 1.0 - chi[m]
    pv = sum(beta_d^k for k in 0:5)
    for ig in 1:2, n in 1:NN
        t_phi_u[ig,n] = chi[ig]*(phi_u*pv)
        t_phi_c[ig,n] = 0.5*(1+alpha)*(phi_c*pv)
    end
    for n in 1:NN
        t_phi_u[f,n] -= (1+0.5alpha)*sub_[5,n]+0.5alpha*sub_[6,n]
        t_phi_u[m,n] -= (1+0.5alpha)*sub_[6,n]+0.5alpha*sub_[5,n]
    end
    for ie in 1:2, iw in 1:WW, n in 1:NN
        wyn=w_y*(1-sub_[2,n]); wfn=w_f[iw,ie]*(1-sub_[3,n]+sub_[1,n])
        for ih in 1:2
            hv=Float64(ih-1)
            t_phi[ih,f,n,iw,ie] = t_phi_u[f,n]+t_phi_c[f,n] -
                sub_[1,n]*(w_f[iw,ie]+0.5alpha*(w_f[iw,ie]+1.25w_f_bar[ie])) +
                0.5*(hv*(2+alpha)*wfn+(1-hv)*(1+alpha)*wyn)
            t_phi[ih,m,n,iw,ie] = t_phi_u[m,n]+t_phi_c[m,n] -
                sub_[1,n]*(1.25w_f_bar[ie]+0.5alpha*(w_f[iw,ie]+1.25w_f_bar[ie])) +
                0.5*(hv*alpha*wfn+(1-hv)*(1+alpha)*wyn)
        end
    end
    for ie in 1:2, iw in 1:WW, n in 1:NN
        wyn=w_y*(1-sub_[2,n]); wfn=w_f[iw,ie]*(1-sub_[3,n]+sub_[1,n])
        limited_[f,n,iw,ie] = t_phi_u[f,n]+t_phi_c[f,n]+0.5*(nu[n,2,2,ie]*(2+alpha)*wfn-nu[n,2,1,ie]*(1+alpha)*wyn)/(nu[n,2,2,ie]-nu[n,2,1,ie])
        limited_[m,n,iw,ie] = t_phi_u[m,n]+t_phi_c[m,n]+0.5*(nu[n,2,2,ie]*(1+alpha)*wyn-nu[n,1,2,ie]*alpha*wfn)/(nu[n,2,2,ie]-nu[n,1,2,ie])
    end
end

# ═══════════════════════════════════════════════════════════════
# BACKWARD INDUCTION — DECISIONS
# ═══════════════════════════════════════════════════════════════

function get_decisions!()
    global i_g, X_t, eta_g, h_g
    i_g .= 0; X_t .= 0.0
    for ie in 1:2, iw in 1:WW
        for it in TT:-1:1
            Eη = zeros(2,NN+1); EX = zeros(2,NN+1)
            for n in 1:min(NN,it)
                wyn=w_y*(1-sub_[2,n]); wfn=w_f[iw,ie]*(1-sub_[3,n]+sub_[1,n])
                for is in 1:SS
                    for ig in 1:2; eta_g[ig,is,n] = v_g[ig,is,n,ie]+beta_d*X_t[ig,is,n,it,iw,ie]; end
                    # h decision
                    if wfn <= (1+alpha)/(2+alpha)*wyn;       ht=2
                    elseif wfn > (1+alpha)/alpha*wyn;        ht=1
                    elseif wfn <= wyn
                        ht = (eta_g[f,is,n]>t_phi[1,f,n,iw,ie] && eta_g[f,is,n]<=t_phi[2,f,n,iw,ie] &&
                              eta_g[m,is,n]>limited_[m,n,iw,ie] && limited_commitment) ? 1 : 2
                    else
                        ht = (eta_g[m,is,n]>t_phi[2,m,n,iw,ie] && eta_g[m,is,n]<=t_phi[1,m,n,iw,ie] &&
                              eta_g[f,is,n]>limited_[f,n,iw,ie] && limited_commitment) ? 2 : 1
                    end
                    itf = eta_g[f,is,n]>t_phi[ht,f,n,iw,ie] ? 2 : 1
                    itm = eta_g[m,is,n]>t_phi[ht,m,n,iw,ie] ? 2 : 1
                    i_g[f,is,n,it,iw,ie]=itf; i_g[m,is,n,it,iw,ie]=itm; h_g[is,n,it,iw,ie]=ht
                    prob = nu[n,itf,itm,ie]
                    for ig in 1:2
                        Eη[ig,n] += prob*(eta_g[ig,is,n]-t_phi[ht,ig,n,iw,ie])/SS
                        EX[ig,n] += X_t[ig,is,n,it,iw,ie]/SS
                    end
                end
            end
            if it > 1
                for ig in 1:2, n in 1:min(NN,it), is in 1:SS
                    ht=h_g[is,n,it,iw,ie]
                    prob=nu[n,i_g[f,is,n,it,iw,ie],i_g[m,is,n,it,iw,ie],ie]
                    nn=min(n+1,NN+1)
                    X_t[ig,is,n,it-1,iw,ie] = Eη[ig,nn]-pi_d*prob*(eta_g[ig,is,n]-t_phi[ht,ig,n,iw,ie]) -
                        (1-pi_d)*Eη[ig,n]+beta_d*(pi_d*X_t[ig,is,n,it,iw,ie]+(1-pi_d)*EX[ig,n])
                end
            end
        end
    end
end

# ═══════════════════════════════════════════════════════════════
# FORWARD SIMULATION — DISTRIBUTIONS
# ═══════════════════════════════════════════════════════════════

function get_distributions!()
    global distrib, tot_dist; distrib .= 0.0
    for ie in 1:2
        ief = ie+1
        for iw in 1:WW
            distrib[:,1,1,iw,ief] .= dist_w[iw,ie]/SS
            for it in 1:TT-1
                for n in 1:min(NN,it)
                    eno=0.0; eyes=0.0
                    for is in 1:SS
                        d=distrib[is,n,it,iw,ief]
                        p=nu[n,i_g[f,is,n,it,iw,ie],i_g[m,is,n,it,iw,ie],ie]
                        eyes += p*d/SS
                        distrib[is,n,it+1,iw,ief] += (1-p)*d*pi_d
                        eno += (1-p)*d/SS*(1-pi_d)
                    end
                    distrib[:,n,  it+1,iw,ief] .+= eno
                    distrib[:,n+1,it+1,iw,ief] .+= eyes
                end
                distrib[:,NN+1,it+1,iw,ief] .+= distrib[:,NN+1,it,iw,ief]
            end
        end
        for n in 1:NN+1; tot_dist[n,ief]=sum(distrib[:,n,TT,:,ief]); end
    end
    distrib[:,:,:,:,al] .= dist_hc[1].*distrib[:,:,:,:,hs].+dist_hc[2].*distrib[:,:,:,:,co]
    tot_dist[:,al] .= dist_hc[1].*tot_dist[:,hs].+dist_hc[2].*tot_dist[:,co]
end

# ═══════════════════════════════════════════════════════════════
# AGGREGATION
# ═══════════════════════════════════════════════════════════════

function get_aggregate!()
    global fert_int, fert_rat, fert_out, fert_age_first, fert_age_last
    fert_int.=0; fert_rat.=0; fert_out.=0; fert_age_first.=0; fert_age_last.=0
    daf=zeros(2); dal=zeros(2)
    for ie in 1:2
        ief=ie+1
        for iw in 1:WW, it in 1:TT, n in 1:min(NN,it), is in 1:SS
            d=distrib[is,n,it,iw,ief]; itf=i_g[f,is,n,it,iw,ie]; itm=i_g[m,is,n,it,iw,ie]
            fert_int[itf,itm,1,ief] += d;  fert_int[itf,itm,n+1,ief] += d
            p=nu[n,itf,itm,ie]; fert_rat[ief] += p*d
            if n==1;  fert_age_first[ief]+=p*d*it; daf[ie]+=p*d; end
            if n==NN; fert_age_last[ief] +=p*d*it; dal[ie]+=p*d; end
        end
        daf[ie]>0 && (fert_age_first[ief]=19.5+(fert_age_first[ief]/daf[ie]-1)*3)
        dal[ie]>0 && (fert_age_last[ief] =19.5+(fert_age_last[ief]/dal[ie]-1)*3)
        for idx in 1:NN+2; s=sum(fert_int[:,:,idx,ief]); s>1e-10 && (fert_int[:,:,idx,ief].*=100/s); end
        for n in 1:NN; fert_out[:,:,n+1,ief].=nu[n,:,:,ie].*100; end
    end
    fert_int[:,:,:,al] .= dist_hc[1].*fert_int[:,:,:,hs].+dist_hc[2].*fert_int[:,:,:,co]
    fert_rat[al]=dist_hc[1]*fert_rat[hs]+dist_hc[2]*fert_rat[co]
    fert_age_first[al]=dist_hc[1]*fert_age_first[hs]+dist_hc[2]*fert_age_first[co]
    fert_age_last[al]=dist_hc[1]*fert_age_last[hs]+dist_hc[2]*fert_age_last[co]
    for n in 1:NN
        fert_out[:,:,n+1,al].=dist_hc[1].*nu[n,:,:,1].*100 .+dist_hc[2].*nu[n,:,:,2].*100
    end
end

# ═══════════════════════════════════════════════════════════════
# SOLVE (fast path: skip wages + preferences)
# ═══════════════════════════════════════════════════════════════

"""Re-solve after changing χ or limited_commitment. Skips wages & preferences."""
function resolve!()
    setup_costs!()
    get_decisions!()
    get_distributions!()
    get_aggregate!()
end

"""Full solve including wages + preferences (initial call only)."""
function solve_full!()
    for ie in 1:2; discretize_wages!(ie, p_cost, w_y, w_f_bar[ie], LFP_without_targ[ie]/100); end
    get_preferences!()
    resolve!()
end

# ═══════════════════════════════════════════════════════════════
# DISPLAY
# ═══════════════════════════════════════════════════════════════

function show_results(label="")
    label!="" && println("\n┌─── $label ───")
    @printf("│ TFR          all=%.4f   hs=%.4f   co=%.4f\n", fert_rat[al], fert_rat[hs], fert_rat[co])
    @printf("│ Age 1st/last %.1f / %.1f\n", fert_age_first[al], fert_age_last[al])
    @printf("│ χ_m=%.4f  χ_f=%.4f  limited_commit=%s\n", chi[m], chi[f], limited_commitment)
    for ief in [hs,co]
        en = ief==hs ? "HS" : "CO"
        println("│ Intentions ($en):")
        for n in 1:NN
            @printf("│   n=%d  NN=%.1f  SY_HN=%.1f  SN_HY=%.1f  YY=%.1f\n",
                n-1, fert_int[1,1,n+1,ief], fert_int[2,1,n+1,ief],
                fert_int[1,2,n+1,ief], fert_int[2,2,n+1,ief])
        end
    end
    @printf("│ Fertility dist (all): n0=%.3f n1=%.3f n2=%.3f n3=%.3f\n",
        tot_dist[1,al], tot_dist[2,al], tot_dist[3,al], tot_dist[4,al])
    println("└───")
end

# ═══════════════════════════════════════════════════════════════
# BENCHMARK PARAMETERS
# ═══════════════════════════════════════════════════════════════

function load_benchmark!()
    global alpha,beta_d,chi_base,phi_c,phi_u,w_f_bar,dist_hc,mu,sig,rho,pi_d
    global nu,chi,w_y,w_y_base,p_cost,LFP_without_targ,realizations
    alpha=0.4; beta_d=0.95; chi_base=0.30673904; phi_c=5000/30000; phi_u=1.0
    w_f_bar.=[1.0,1.5]; dist_hc[2]=0.2534; dist_hc[1]=1-dist_hc[2]
    LFP_without_targ.=[62.59615,80.49617]
    # conception — HS
    nu[1,1,:,1].=[.1789176717197569,.1789176717197569]; nu[1,2,:,1].=[.1789176717197569,.4021394778241374]
    nu[2,1,:,1].=[.1305865488254912,.1305865488254912]; nu[2,2,:,1].=[.2360278539287936,.3983988978195017]
    nu[3,1,:,1].=[.0428344406053852,.0428344406053852]; nu[3,2,:,1].=[.1220761109723249,.3615216294526126]
    # conception — CO
    nu[1,1,:,2].=[.1702864617346444,.1702864617346444]; nu[1,2,:,2].=[.1702864617346444,.4377648898880003]
    nu[2,1,:,2].=[.1142364241909013,.1142364241909013]; nu[2,2,:,2].=[.2666842193124367,.4247963593590841]
    nu[3,1,:,2].=[.024787697107474,.024787697107474];   nu[3,2,:,2].=[.024787697107474,.3090825559183003]
    # preferences
    mu[1,f,1]=5.06895443;  mu[1,m,1]=3.63503709
    mu[2,f,1]=1.78695119;  mu[2,m,1]=-6.44256185
    mu[3,f,1]=-0.15113675; mu[3,m,1]=-15.53940557
    mu[1,f,2]=5.77527295;  mu[1,m,2]=4.84667182
    mu[2,f,2]=3.05908870;  mu[2,m,2]=-0.00001244
    mu[3,f,2]=0.05275858;  mu[3,m,2]=-14.63174255
    sig[f]=3.07058582^2; sig[m]=12.72206793^2
    rho=0.80214353; pi_d=0.28971537; w_y_base=0.58407903; p_cost=0.35808630
    chi[m]=chi_base; chi[f]=1-chi[m]; w_y=w_y_base
    # shocks
    if isfile("input/random.inp")
        println("Loading shocks from input/random.inp")
        open("input/random.inp") do io
            for is in 1:SS; realizations[is,:].=parse.(Float64,split(readline(io)))[1:2]; end
        end
    else
        println("Generating new shocks (seed=12345)")
        Random.seed!(12345)
        for is in 1:SS÷2; i1=2(is-1)+1; realizations[i1,:].=rand(2); realizations[i1+1,:].=1 .-realizations[i1,:]; end
    end
end

# ═══════════════════════════════════════════════════════════════
# EXPERIMENTS — edit below
# ═══════════════════════════════════════════════════════════════

function main()
    load_benchmark!()

    println("Solving baseline (full)...")
    @time solve_full!()
    show_results("BASELINE  χ_m=$(round(chi[m],digits=4))")

    # ── 1. Raise male cost share ──────────────────────────────
    chi[m] = 0.45; chi[f] = 1 - chi[m]
    println("\nRe-solving with χ_m = 0.45 ...")
    @time resolve!()
    show_results("χ_m = 0.45  (fathers bear more cost)")

    # ── 2. Equal sharing ──────────────────────────────────────
    chi[m] = 0.50; chi[f] = 0.50
    println("\nRe-solving with χ_m = 0.50 ...")
    @time resolve!()
    show_results("χ_m = 0.50  (equal sharing)")

    # ── 3. Turn off limited commitment ────────────────────────
    global limited_commitment
    chi[m] = chi_base; chi[f] = 1-chi[m]   # restore baseline χ
    limited_commitment = false
    println("\nRe-solving without limited commitment ...")
    @time resolve!()
    show_results("No limited commitment (baseline χ)")

    limited_commitment = true   # restore

    # ── 4. Sweep χ_m from 0.15 to 0.55 ───────────────────────
    println("\n┌─── Sweep χ_m ───")
    @printf("│ %6s  %8s  %8s  %8s  %8s  %8s\n", "χ_m", "TFR_all", "TFR_hs", "TFR_co", "BothYes%", "SheBlk%")
    println("│ " * "─"^58)
    for χm in 0.15:0.05:0.55
        chi[m] = χm; chi[f] = 1-χm
        resolve!()
        @printf("│ %6.3f  %8.4f  %8.4f  %8.4f  %8.2f  %8.2f\n",
            χm, fert_rat[al], fert_rat[hs], fert_rat[co],
            fert_int[2,2,2,al],    # both-yes at parity 0
            fert_int[2,1,2,al])    # she-yes-he-no at parity 0 ("she blocked")
    end
    println("└───")

    # restore
    chi[m] = chi_base; chi[f] = 1-chi[m]
end

main()

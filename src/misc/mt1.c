/* mt1.c (0-1 knapsack problem; Martello & Toth algorithm) */

/***********************************************************************
*  This code is part of GLPK (GNU Linear Programming Kit).
*
*  THIS CODE IS THE RESULT OF TRANSLATION OF THE FORTRAN SUBROUTINES
*  MT1 FROM THE BOOK:
*
*  SILVANO MARTELLO, PAOLO TOTH. KNAPSACK PROBLEMS: ALGORITHMS AND
*  COMPUTER IMPLEMENTATIONS. JOHN WILEY & SONS, 1990.
*
*  THE TRANSLATION HAS BEEN DONE WITH THE PERMISSION OF THE AUTHORS OF
*  THE ORIGINAL FORTRAN SUBROUTINES: SILVANO MARTELLO AND PAOLO TOTH.
*
*  The translation was made by Andrew Makhorin <mao@gnu.org>.
*
*  GLPK is free software: you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by
*  the Free Software Foundation, either version 3 of the License, or
*  (at your option) any later version.
*
*  GLPK is distributed in the hope that it will be useful, but WITHOUT
*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
*  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
*  License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with GLPK. If not, see <http://www.gnu.org/licenses/>.
***********************************************************************/

/*  -- translated by f2c (version 20100827).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#if 0 /* by mao */
#include "f2c.h"
#else
#include "env.h"
#include "mt1.h"

typedef int integer;
typedef float real;
#endif

/*<       SUBROUTINE MT1(N,P,W,C,Z,X,JDIM,JCK,XX,MIN,PSIGN,WSIGN,ZSIGN) >*/
#if 1 /* by mao */
static int chmt1_(int *, int *, int *, int *, int *, int *);

static
#endif
/* Subroutine */ int mt1_(integer *n, integer *p, integer *w, integer *c__,
	integer *z__, integer *x, integer *jdim, integer *jck, integer *xx,
	integer *min__, integer *psign, integer *wsign, integer *zsign)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static real a, b;
    static integer j, r__, t, j1, n1, ch, ii, jj, kk, in, ll, ip, nn, iu, ii1,
	     chs, lim, lim1, diff, lold, mink;
    extern /* Subroutine */ int chmt1_(integer *, integer *, integer *,
	    integer *, integer *, integer *);
    static integer profit;


/* THIS SUBROUTINE SOLVES THE 0-1 SINGLE KNAPSACK PROBLEM */

/* MAXIMIZE  Z = P(1)*X(1) + ... + P(N)*X(N) */

/* SUBJECT TO:   W(1)*X(1) + ... + W(N)*X(N) .LE. C , */
/*               X(J) = 0 OR 1  FOR J=1,...,N. */

/* THE PROGRAM IS INCLUDED IN THE VOLUME */
/*   S. MARTELLO, P. TOTH, "KNAPSACK PROBLEMS: ALGORITHMS */
/*   AND COMPUTER IMPLEMENTATIONS", JOHN WILEY, 1990 */
/* AND IMPLEMENTS THE BRANCH-AND-BOUND ALGORITHM DESCRIBED IN */
/* SECTION  2.5.2 . */
/* THE PROGRAM DERIVES FROM AN EARLIER CODE PRESENTED IN */
/*  S. MARTELLO, P. TOTH, "ALGORITHM FOR THE SOLUTION OF THE 0-1 SINGLE */
/*  KNAPSACK PROBLEM", COMPUTING, 1978. */

/* THE INPUT PROBLEM MUST SATISFY THE CONDITIONS */

/*   1) 2 .LE. N .LE. JDIM - 1 ; */
/*   2) P(J), W(J), C  POSITIVE INTEGERS; */
/*   3) MAX (W(J)) .LE. C ; */
/*   4) W(1) + ... + W(N) .GT. C ; */
/*   5) P(J)/W(J) .GE. P(J+1)/W(J+1) FOR J=1,...,N-1. */

/* MT1 CALLS  1  PROCEDURE: CHMT1. */

/* THE PROGRAM IS COMPLETELY SELF-CONTAINED AND COMMUNICATION TO IT IS */
/* ACHIEVED SOLELY THROUGH THE PARAMETER LIST OF MT1. */
/* NO MACHINE-DEPENDENT CONSTANT IS USED. */
/* THE PROGRAM IS WRITTEN IN 1967 AMERICAN NATIONAL STANDARD FORTRAN */
/* AND IS ACCEPTED BY THE PFORT VERIFIER (PFORT IS THE PORTABLE */
/* SUBSET OF ANSI DEFINED BY THE ASSOCIATION FOR COMPUTING MACHINERY). */
/* THE PROGRAM HAS BEEN TESTED ON A DIGITAL VAX 11/780 AND AN H.P. */
/* 9000/840. */

/* MT1 NEEDS  8  ARRAYS ( P ,  W ,  X ,  XX ,  MIN ,  PSIGN ,  WSIGN */
/*                        AND  ZSIGN ) OF LENGTH AT LEAST  N + 1 . */

/* MEANING OF THE INPUT PARAMETERS: */
/* N    = NUMBER OF ITEMS; */
/* P(J) = PROFIT OF ITEM  J  (J=1,...,N); */
/* W(J) = WEIGHT OF ITEM  J  (J=1,...,N); */
/* C    = CAPACITY OF THE KNAPSACK; */
/* JDIM = DIMENSION OF THE 8 ARRAYS; */
/* JCK  = 1 IF CHECK ON THE INPUT DATA IS DESIRED, */
/*      = 0 OTHERWISE. */

/* MEANING OF THE OUTPUT PARAMETERS: */
/* Z    = VALUE OF THE OPTIMAL SOLUTION IF  Z .GT. 0 , */
/*      = ERROR IN THE INPUT DATA (WHEN JCK=1) IF Z .LT. 0 : CONDI- */
/*        TION  - Z  IS VIOLATED; */
/* X(J) = 1 IF ITEM  J  IS IN THE OPTIMAL SOLUTION, */
/*      = 0 OTHERWISE. */

/* ARRAYS XX, MIN, PSIGN, WSIGN AND ZSIGN ARE DUMMY. */

/* ALL THE PARAMETERS ARE INTEGER. ON RETURN OF MT1 ALL THE INPUT */
/* PARAMETERS ARE UNCHANGED. */

/*<       INTEGER P(JDIM),W(JDIM),X(JDIM),C,Z >*/
/*<       INTEGER XX(JDIM),MIN(JDIM),PSIGN(JDIM),WSIGN(JDIM),ZSIGN(JDIM) >*/
/*<       INTEGER CH,CHS,DIFF,PROFIT,R,T >*/
/*<       Z = 0 >*/
    /* Parameter adjustments */
    --zsign;
    --wsign;
    --psign;
    --min__;
    --xx;
    --x;
    --w;
    --p;

    /* Function Body */
    *z__ = 0;
/*<       IF ( JCK .EQ. 1 ) CALL CHMT1(N,P,W,C,Z,JDIM) >*/
    if (*jck == 1) {
	chmt1_(n, &p[1], &w[1], c__, z__, jdim);
    }
/*<       IF ( Z .LT. 0 ) RETURN >*/
    if (*z__ < 0) {
	return 0;
    }
/* INITIALIZE. */
/*<       CH = C >*/
    ch = *c__;
/*<       IP = 0 >*/
    ip = 0;
/*<       CHS = CH >*/
    chs = ch;
/*<       DO 10 LL=1,N >*/
    i__1 = *n;
    for (ll = 1; ll <= i__1; ++ll) {
/*<         IF ( W(LL) .GT. CHS ) GO TO 20 >*/
	if (w[ll] > chs) {
	    goto L20;
	}
/*<         IP = IP + P(LL) >*/
	ip += p[ll];
/*<         CHS = CHS - W(LL) >*/
	chs -= w[ll];
/*<    10 CONTINUE >*/
/* L10: */
    }
/*<    20 LL = LL - 1 >*/
L20:
    --ll;
/*<       IF ( CHS .EQ. 0 ) GO TO 50 >*/
    if (chs == 0) {
	goto L50;
    }
/*<       P(N+1) = 0 >*/
    p[*n + 1] = 0;
/*<       W(N+1) = CH + 1 >*/
    w[*n + 1] = ch + 1;
/*<       LIM = IP + CHS*P(LL+2)/W(LL+2) >*/
    lim = ip + chs * p[ll + 2] / w[ll + 2];
/*<       A = W(LL+1) - CHS >*/
    a = (real) (w[ll + 1] - chs);
/*<       B = IP + P(LL+1) >*/
    b = (real) (ip + p[ll + 1]);
/*<       LIM1 = B - A*FLOAT(P(LL))/FLOAT(W(LL)) >*/
    lim1 = b - a * (real) p[ll] / (real) w[ll];
/*<       IF ( LIM1 .GT. LIM ) LIM = LIM1 >*/
    if (lim1 > lim) {
	lim = lim1;
    }
/*<       MINK = CH + 1 >*/
    mink = ch + 1;
/*<       MIN(N) = MINK >*/
    min__[*n] = mink;
/*<       DO 30 J=2,N >*/
    i__1 = *n;
    for (j = 2; j <= i__1; ++j) {
/*<         KK = N + 2 - J >*/
	kk = *n + 2 - j;
/*<         IF ( W(KK) .LT. MINK ) MINK = W(KK) >*/
	if (w[kk] < mink) {
	    mink = w[kk];
	}
/*<         MIN(KK-1) = MINK >*/
	min__[kk - 1] = mink;
/*<    30 CONTINUE >*/
/* L30: */
    }
/*<       DO 40 J=1,N >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<         XX(J) = 0 >*/
	xx[j] = 0;
/*<    40 CONTINUE >*/
/* L40: */
    }
/*<       Z = 0 >*/
    *z__ = 0;
/*<       PROFIT = 0 >*/
    profit = 0;
/*<       LOLD = N >*/
    lold = *n;
/*<       II = 1 >*/
    ii = 1;
/*<       GO TO 170 >*/
    goto L170;
/*<    50 Z = IP >*/
L50:
    *z__ = ip;
/*<       DO 60 J=1,LL >*/
    i__1 = ll;
    for (j = 1; j <= i__1; ++j) {
/*<         X(J) = 1 >*/
	x[j] = 1;
/*<    60 CONTINUE >*/
/* L60: */
    }
/*<       NN = LL + 1 >*/
    nn = ll + 1;
/*<       DO 70 J=NN,N >*/
    i__1 = *n;
    for (j = nn; j <= i__1; ++j) {
/*<         X(J) = 0 >*/
	x[j] = 0;
/*<    70 CONTINUE >*/
/* L70: */
    }
/*<       RETURN >*/
    return 0;
/* TRY TO INSERT THE II-TH ITEM INTO THE CURRENT SOLUTION. */
/*<    80 IF ( W(II) .LE. CH ) GO TO 90 >*/
L80:
    if (w[ii] <= ch) {
	goto L90;
    }
/*<       II1 = II + 1 >*/
    ii1 = ii + 1;
/*<       IF ( Z .GE. CH*P(II1)/W(II1) + PROFIT ) GO TO 280 >*/
    if (*z__ >= ch * p[ii1] / w[ii1] + profit) {
	goto L280;
    }
/*<       II = II1 >*/
    ii = ii1;
/*<       GO TO 80 >*/
    goto L80;
/* BUILD A NEW CURRENT SOLUTION. */
/*<    90 IP = PSIGN(II) >*/
L90:
    ip = psign[ii];
/*<       CHS = CH - WSIGN(II) >*/
    chs = ch - wsign[ii];
/*<       IN = ZSIGN(II) >*/
    in = zsign[ii];
/*<       DO 100 LL=IN,N >*/
    i__1 = *n;
    for (ll = in; ll <= i__1; ++ll) {
/*<         IF ( W(LL) .GT. CHS ) GO TO 160 >*/
	if (w[ll] > chs) {
	    goto L160;
	}
/*<         IP = IP + P(LL) >*/
	ip += p[ll];
/*<         CHS = CHS - W(LL) >*/
	chs -= w[ll];
/*<   100 CONTINUE >*/
/* L100: */
    }
/*<       LL = N >*/
    ll = *n;
/*<   110 IF ( Z .GE. IP + PROFIT ) GO TO 280 >*/
L110:
    if (*z__ >= ip + profit) {
	goto L280;
    }
/*<       Z = IP + PROFIT >*/
    *z__ = ip + profit;
/*<       NN = II - 1 >*/
    nn = ii - 1;
/*<       DO 120 J=1,NN >*/
    i__1 = nn;
    for (j = 1; j <= i__1; ++j) {
/*<         X(J) = XX(J) >*/
	x[j] = xx[j];
/*<   120 CONTINUE >*/
/* L120: */
    }
/*<       DO 130 J=II,LL >*/
    i__1 = ll;
    for (j = ii; j <= i__1; ++j) {
/*<         X(J) = 1 >*/
	x[j] = 1;
/*<   130 CONTINUE >*/
/* L130: */
    }
/*<       IF ( LL .EQ. N ) GO TO 150 >*/
    if (ll == *n) {
	goto L150;
    }
/*<       NN = LL + 1 >*/
    nn = ll + 1;
/*<       DO 140 J=NN,N >*/
    i__1 = *n;
    for (j = nn; j <= i__1; ++j) {
/*<         X(J) = 0 >*/
	x[j] = 0;
/*<   140 CONTINUE >*/
/* L140: */
    }
/*<   150 IF ( Z .NE. LIM ) GO TO 280 >*/
L150:
    if (*z__ != lim) {
	goto L280;
    }
/*<       RETURN >*/
    return 0;
/*<   160 IU = CHS*P(LL)/W(LL) >*/
L160:
    iu = chs * p[ll] / w[ll];
/*<       LL = LL - 1 >*/
    --ll;
/*<       IF ( IU .EQ. 0 ) GO TO 110 >*/
    if (iu == 0) {
	goto L110;
    }
/*<       IF ( Z .GE. PROFIT + IP + IU ) GO TO 280 >*/
    if (*z__ >= profit + ip + iu) {
	goto L280;
    }
/* SAVE THE CURRENT SOLUTION. */
/*<   170 WSIGN(II) = CH - CHS >*/
L170:
    wsign[ii] = ch - chs;
/*<       PSIGN(II) = IP >*/
    psign[ii] = ip;
/*<       ZSIGN(II) = LL + 1 >*/
    zsign[ii] = ll + 1;
/*<       XX(II) = 1 >*/
    xx[ii] = 1;
/*<       NN = LL - 1 >*/
    nn = ll - 1;
/*<       IF ( NN .LT. II) GO TO 190 >*/
    if (nn < ii) {
	goto L190;
    }
/*<       DO 180 J=II,NN >*/
    i__1 = nn;
    for (j = ii; j <= i__1; ++j) {
/*<         WSIGN(J+1) = WSIGN(J) - W(J) >*/
	wsign[j + 1] = wsign[j] - w[j];
/*<         PSIGN(J+1) = PSIGN(J) - P(J) >*/
	psign[j + 1] = psign[j] - p[j];
/*<         ZSIGN(J+1) = LL + 1 >*/
	zsign[j + 1] = ll + 1;
/*<         XX(J+1) = 1 >*/
	xx[j + 1] = 1;
/*<   180 CONTINUE >*/
/* L180: */
    }
/*<   190 J1 = LL + 1 >*/
L190:
    j1 = ll + 1;
/*<       DO 200 J=J1,LOLD >*/
    i__1 = lold;
    for (j = j1; j <= i__1; ++j) {
/*<         WSIGN(J) = 0 >*/
	wsign[j] = 0;
/*<         PSIGN(J) = 0 >*/
	psign[j] = 0;
/*<         ZSIGN(J) = J >*/
	zsign[j] = j;
/*<   200 CONTINUE >*/
/* L200: */
    }
/*<       LOLD = LL >*/
    lold = ll;
/*<       CH = CHS >*/
    ch = chs;
/*<       PROFIT = PROFIT + IP >*/
    profit += ip;
/*<       IF ( LL - (N - 2) ) 240, 220, 210 >*/
    if ((i__1 = ll - (*n - 2)) < 0) {
	goto L240;
    } else if (i__1 == 0) {
	goto L220;
    } else {
	goto L210;
    }
/*<   210 II = N >*/
L210:
    ii = *n;
/*<       GO TO 250 >*/
    goto L250;
/*<   220 IF ( CH .LT. W(N) ) GO TO 230 >*/
L220:
    if (ch < w[*n]) {
	goto L230;
    }
/*<       CH = CH - W(N) >*/
    ch -= w[*n];
/*<       PROFIT = PROFIT + P(N) >*/
    profit += p[*n];
/*<       XX(N) = 1 >*/
    xx[*n] = 1;
/*<   230 II = N - 1 >*/
L230:
    ii = *n - 1;
/*<       GO TO 250 >*/
    goto L250;
/*<   240 II = LL + 2 >*/
L240:
    ii = ll + 2;
/*<       IF ( CH .GE. MIN(II-1) ) GO TO 80 >*/
    if (ch >= min__[ii - 1]) {
	goto L80;
    }
/* SAVE THE CURRENT OPTIMAL SOLUTION. */
/*<   250 IF ( Z .GE. PROFIT ) GO TO 270 >*/
L250:
    if (*z__ >= profit) {
	goto L270;
    }
/*<       Z = PROFIT >*/
    *z__ = profit;
/*<       DO 260 J=1,N >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<         X(J) = XX(J) >*/
	x[j] = xx[j];
/*<   260 CONTINUE >*/
/* L260: */
    }
/*<       IF ( Z .EQ. LIM ) RETURN >*/
    if (*z__ == lim) {
	return 0;
    }
/*<   270 IF ( XX(N) .EQ. 0 ) GO TO 280 >*/
L270:
    if (xx[*n] == 0) {
	goto L280;
    }
/*<       XX(N) = 0 >*/
    xx[*n] = 0;
/*<       CH = CH + W(N) >*/
    ch += w[*n];
/*<       PROFIT = PROFIT - P(N) >*/
    profit -= p[*n];
/* BACKTRACK. */
/*<   280 NN = II - 1 >*/
L280:
    nn = ii - 1;
/*<       IF ( NN .EQ. 0 ) RETURN >*/
    if (nn == 0) {
	return 0;
    }
/*<       DO 290 J=1,NN >*/
    i__1 = nn;
    for (j = 1; j <= i__1; ++j) {
/*<         KK = II - J >*/
	kk = ii - j;
/*<         IF ( XX(KK) .EQ. 1 ) GO TO 300 >*/
	if (xx[kk] == 1) {
	    goto L300;
	}
/*<   290 CONTINUE >*/
/* L290: */
    }
/*<       RETURN >*/
    return 0;
/*<   300 R = CH >*/
L300:
    r__ = ch;
/*<       CH = CH + W(KK) >*/
    ch += w[kk];
/*<       PROFIT = PROFIT - P(KK) >*/
    profit -= p[kk];
/*<       XX(KK) = 0 >*/
    xx[kk] = 0;
/*<       IF ( R .LT. MIN(KK) ) GO TO 310 >*/
    if (r__ < min__[kk]) {
	goto L310;
    }
/*<       II = KK + 1 >*/
    ii = kk + 1;
/*<       GO TO 80 >*/
    goto L80;
/*<   310 NN = KK + 1 >*/
L310:
    nn = kk + 1;
/*<       II = KK >*/
    ii = kk;
/* TRY TO SUBSTITUTE THE NN-TH ITEM FOR THE KK-TH. */
/*<   320 IF ( Z .GE. PROFIT + CH*P(NN)/W(NN) ) GO TO 280 >*/
L320:
    if (*z__ >= profit + ch * p[nn] / w[nn]) {
	goto L280;
    }
/*<       DIFF = W(NN) - W(KK) >*/
    diff = w[nn] - w[kk];
/*<       IF ( DIFF ) 370, 330, 340 >*/
    if (diff < 0) {
	goto L370;
    } else if (diff == 0) {
	goto L330;
    } else {
	goto L340;
    }
/*<   330 NN = NN + 1 >*/
L330:
    ++nn;
/*<       GO TO 320 >*/
    goto L320;
/*<   340 IF ( DIFF .GT. R ) GO TO 330 >*/
L340:
    if (diff > r__) {
	goto L330;
    }
/*<       IF ( Z .GE. PROFIT + P(NN) ) GO TO 330 >*/
    if (*z__ >= profit + p[nn]) {
	goto L330;
    }
/*<       Z = PROFIT + P(NN) >*/
    *z__ = profit + p[nn];
/*<       DO 350 J=1,KK >*/
    i__1 = kk;
    for (j = 1; j <= i__1; ++j) {
/*<         X(J) = XX(J) >*/
	x[j] = xx[j];
/*<   350 CONTINUE >*/
/* L350: */
    }
/*<       JJ = KK + 1 >*/
    jj = kk + 1;
/*<       DO 360 J=JJ,N >*/
    i__1 = *n;
    for (j = jj; j <= i__1; ++j) {
/*<         X(J) = 0 >*/
	x[j] = 0;
/*<   360 CONTINUE >*/
/* L360: */
    }
/*<       X(NN) = 1 >*/
    x[nn] = 1;
/*<       IF ( Z .EQ. LIM ) RETURN >*/
    if (*z__ == lim) {
	return 0;
    }
/*<       R = R - DIFF >*/
    r__ -= diff;
/*<       KK = NN >*/
    kk = nn;
/*<       NN = NN + 1 >*/
    ++nn;
/*<       GO TO 320 >*/
    goto L320;
/*<   370 T = R - DIFF >*/
L370:
    t = r__ - diff;
/*<       IF ( T .LT. MIN(NN) ) GO TO 330 >*/
    if (t < min__[nn]) {
	goto L330;
    }
/*<       IF ( Z .GE. PROFIT + P(NN) + T*P(NN+1)/W(NN+1)) GO TO 280 >*/
    if (*z__ >= profit + p[nn] + t * p[nn + 1] / w[nn + 1]) {
	goto L280;
    }
/*<       CH = CH - W(NN) >*/
    ch -= w[nn];
/*<       PROFIT = PROFIT + P(NN) >*/
    profit += p[nn];
/*<       XX(NN) = 1 >*/
    xx[nn] = 1;
/*<       II = NN + 1 >*/
    ii = nn + 1;
/*<       WSIGN(NN) = W(NN) >*/
    wsign[nn] = w[nn];
/*<       PSIGN(NN) = P(NN) >*/
    psign[nn] = p[nn];
/*<       ZSIGN(NN) = II >*/
    zsign[nn] = ii;
/*<       N1 = NN + 1 >*/
    n1 = nn + 1;
/*<       DO 380 J=N1,LOLD >*/
    i__1 = lold;
    for (j = n1; j <= i__1; ++j) {
/*<         WSIGN(J) = 0 >*/
	wsign[j] = 0;
/*<         PSIGN(J) = 0 >*/
	psign[j] = 0;
/*<         ZSIGN(J) = J >*/
	zsign[j] = j;
/*<   380 CONTINUE >*/
/* L380: */
    }
/*<       LOLD = NN >*/
    lold = nn;
/*<       GO TO 80 >*/
    goto L80;
/*<       END >*/
} /* mt1_ */

/*<       SUBROUTINE CHMT1(N,P,W,C,Z,JDIM) >*/
#if 1 /* by mao */
static
#endif
/* Subroutine */ int chmt1_(integer *n, integer *p, integer *w, integer *c__,
	integer *z__, integer *jdim)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer j;
    static real r__, rr;
    static integer jsw;


/* CHECK THE INPUT DATA. */

/*<       INTEGER P(JDIM),W(JDIM),C,Z >*/
/*<       IF ( N .GE. 2 .AND. N .LE. JDIM - 1 ) GO TO 10 >*/
    /* Parameter adjustments */
    --w;
    --p;

    /* Function Body */
    if (*n >= 2 && *n <= *jdim - 1) {
	goto L10;
    }
/*<       Z = - 1 >*/
    *z__ = -1;
/*<       RETURN >*/
    return 0;
/*<    10 IF ( C .GT. 0 ) GO TO 30 >*/
L10:
    if (*c__ > 0) {
	goto L30;
    }
/*<    20 Z = - 2 >*/
L20:
    *z__ = -2;
/*<       RETURN >*/
    return 0;
/*<    30 JSW = 0 >*/
L30:
    jsw = 0;
/*<       RR = FLOAT(P(1))/FLOAT(W(1)) >*/
    rr = (real) p[1] / (real) w[1];
/*<       DO 50 J=1,N >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<         R = RR >*/
	r__ = rr;
/*<         IF ( P(J) .LE. 0 ) GO TO 20 >*/
	if (p[j] <= 0) {
	    goto L20;
	}
/*<         IF ( W(J) .LE. 0 ) GO TO 20 >*/
	if (w[j] <= 0) {
	    goto L20;
	}
/*<         JSW = JSW + W(J) >*/
	jsw += w[j];
/*<         IF ( W(J) .LE. C ) GO TO 40 >*/
	if (w[j] <= *c__) {
	    goto L40;
	}
/*<         Z = - 3 >*/
	*z__ = -3;
/*<         RETURN >*/
	return 0;
/*<    40   RR = FLOAT(P(J))/FLOAT(W(J)) >*/
L40:
	rr = (real) p[j] / (real) w[j];
/*<         IF ( RR .LE. R ) GO TO 50 >*/
	if (rr <= r__) {
	    goto L50;
	}
/*<         Z = - 5 >*/
	*z__ = -5;
/*<         RETURN >*/
	return 0;
/*<    50 CONTINUE >*/
L50:
	;
    }
/*<       IF ( JSW .GT. C ) RETURN >*/
    if (jsw > *c__) {
	return 0;
    }
/*<       Z = - 4 >*/
    *z__ = -4;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* chmt1_ */

#if 1 /* by mao */
int mt1(int n, int p[], int w[], int c, int x[], int jck, int xx[],
      int min[], int psign[], int wsign[], int zsign[])
{     /* solve 0-1 knapsack problem */
      int z, jdim = n+1, j, s1, s2;
      mt1_(&n, &p[1], &w[1], &c, &z, &x[1], &jdim, &jck, &xx[1],
         &min[1], &psign[1], &wsign[1], &zsign[1]);
      /* check solution found */
      s1 = s2 = 0;
      for (j = 1; j <= n; j++)
      {  xassert(x[j] == 0 || x[j] == 1);
         if (x[j])
            s1 += p[j], s2 += w[j];
      }
      xassert(s1 == z);
      xassert(s2 <= c);
      return z;
}
#endif

/* eof */

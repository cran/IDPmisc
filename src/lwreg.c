/*
 * This is a C-program called by R. It is the core piece in a lowess-like
 * smoothing procedure. The idea is borrowed from Cleveland's FORTRAN
 * implementaion (see lowess.f on STATLIB).
 *
 * ARu, April 1999; modified April 2004
 *
 * Compiled by R CMD SHLIB locreg.c (2004)
 */

/* -----------------------  Some definitions  --------------------------- */

#include <R.h>     /* R header  */

#define	min(a, b)	( ((a) < (b)) ? (a) : (b) )
#define	max(a, b)	( ((a) > (b)) ? (a) : (b) )
#define	sign(a)		( ((a) == 0) ? 0 : (((a) > 0) ? 1 : -1) )

/* -------------------- lowreg is called by R ---------------------- */

/*       Purpose
 *
 *       LWREG computes the smooth of a scatterplot of Y  against X  using
 *       locally  weighted regression, where the weights may include robust
 *       weights.  Fitted values, yfit, are computed at each of the  values
 *       of  the  horizontal axis in x.

 *   Parameter - passing:
 *       x = Input; abscissas of the points on the scatterplot;
 *           the values in X must be ordered from smallest to largest.
 *       y = Input; ordinates of the points on the scatterplot.
 *       n = Input; dimension of x, y, ow, and yfit.
 *       f = Input; specifies the amount of smoothing; f is the number of
 *           points used to compute each fitted value.
 *   delta = Input; nonnegative parameter which may be used to save
 *           computations.
 *      ow = Input; Weights; ow(i) is the weight given to the point
 *           (x(i), y(i)); if neither robustness nor heteroscedasticy is
 *           considered, these weights are all 1.
 *    yfit = Output; fitted values; yfit(i) is the fitted value at x(i).
 *
 */

void lwreg(double x[], double y[], int *n, int *f, double *delta,
	   double ow[], double yfit[])

{
  int nleft, nright, nrt, last, icp, j;
  double range, d1, d2, denom, sow, a, b, c, r, cut, h, h9, h1, *w;

  /* w = Salloc(*n, double); */
  w = (double *) R_alloc(*n, sizeof(double));

/* ------------------------------ body ---------------------------------- */

  if (*n < 2){ yfit[0] = y[0]; return; };  /* Just in case ... */

  range = x[*n-1]-x[0];
  nleft = 0; nright = *f-1;
  last = -1;       /* index of prev estimated point */
  icp = 0;        /* index of current point */
  do { /* beginning of repeat-until loop */
    while(nright < *n-1){
      /*  move nleft, nright to right if radius decreases */
      d1 = x[icp] - x[nleft];
      d2 = x[nright+1] - x[icp];
      /* if d1<=d2 with x(nright+1)==x(nright), lowest fixes */
      if (d1 <= d2) break;
      /* radius will not decrease by move right */
      nleft = nleft+1;
      nright = nright+1;
    }

    /* The  fitted  value, yfit[i], is computed  at  the  value,  x[i],
     * of  the   horizontal   axis.                                   */
    h = max(x[icp] - x[nleft],x[nright] - x[icp]);
    h9 = .999*h;
    h1 = .001*h;
    sow = 0.0;        /* sum of weights  */
    for(j=nleft; j<*n; j++){
      /* compute kernel weights (pick up all ties on right) */
      w[j] = 0.0;
      r = fabs(x[j]-x[icp]);
      if (r<=h9) {    /* small enough for non-zero weight */
	if (r>h1) w[j] = pow((1.0-pow((r/h),3)),3);
	else      w[j] = 1.0;
	w[j] = ow[j]*w[j];
	sow = sow + w[j];
      }
      else{
	if(x[j] > x[icp]) break;   /* get out at first zero wt on right */
      }
    }
    nrt=j-1;     /* rightmost pt (may be greater than nright because of ties*/
  if (sow <= 0.0) yfit[icp] = y[icp];
  else { /* weighted least squares   */
    for(j = nleft; j <= nrt; j++) w[j] = w[j]/sow; /* make sum of w[j] == 1 */
    if (h > 0.0) { /* use linear fit */
      a = 0.0;
      for(j = nleft; j <= nrt; j++)
	a = a+w[j]*x[j];   /* weighted center of x values */
      b = x[icp] - a;
      c = 0.0;
      for(j = nleft; j <= nrt; j++) c = c + w[j]*(x[j]-a)*(x[j]-a);
      if(sqrt(c)>.001*range) {
	/*  points are spread out enough to compute slope  */
	b = b/c;
	for(j = nleft; j <= nrt; j++) w[j] = w[j]*(1.0+b*(x[j]-a));
      }
    }
    yfit[icp] = 0.0;
    for(j = nleft; j <= nrt; j++) yfit[icp] = yfit[icp]+w[j]*y[j];
  }

    /* Move forward */
    if (last < icp-1) { /* skipped points -- interpolate */
      denom = x[icp] - x[last];    /* non-zero - proof? */
      for(j=last+1; j<icp; j++){
	a = (x[j] - x[last])/denom;
	yfit[j] = a * yfit[icp] + (1.0 - a) * yfit[last];
      }
    }
    last = icp;                        /* last point actually estimated      */
    cut = x[last] + *delta;            /* x coord of close points            */
    for(icp = last+1; icp < *n; icp++){/* find close points                  */
      if(x[icp] > cut) break;          /* i one beyond last point within cut */
      if(x[icp] == x[last]){           /* exact match in x                   */
	yfit[icp] = yfit[last];
	last = icp;
      }
    }
    icp = max(last+1,icp-1);  /* back 1 point so interpolation within delta,
                                 but always go forward                      */
  } while(last < (*n - 1));   /* end of repeat-until loop */

  return;
}


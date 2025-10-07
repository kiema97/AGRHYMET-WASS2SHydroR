test_that("wass2s_class_thr returns Q1 and Q3 by default", {
  q_hist <- 100:199  # longueur 100, déterministe
  th <- wass2s_class_thr(q_hist)  # probs = c(0.25, 0.75) par défaut

  # Valeurs attendues avec type=7 (par défaut de stats::quantile)
  # Q1 = 124.75 ; Q3 = 174.25
  expect_named(th, c("t1","t2"))
  expect_equal(th[["t1"]], 124.75, tolerance = 1e-10)
  expect_equal(th[["t2"]], 174.25, tolerance = 1e-10)
})

test_that("wass2s_class_probs_norm produces valid probabilities that sum to 1", {
  th <- c(t1 = 10, t2 = 20)

  # Cas 1: mu = t1 -> p_below = 0.5
  P1 <- wass2s_class_probs_norm(mu = 10, sigma = 2, thresholds = th)
  expect_true(all(c("p_below","p_normal","p_above") %in% names(P1)))
  expect_equal(P1$p_below, 0.5, tolerance = 1e-12)
  expect_true(abs(sum(P1) - 1) < 1e-12)

  # Cas 2: mu = t2 -> p_above = 0.5
  P2 <- wass2s_class_probs_norm(mu = 20, sigma = 3, thresholds = th)
  expect_equal(P2$p_above, 0.5, tolerance = 1e-12)
  expect_true(abs(sum(P2) - 1) < 1e-12)

  # Cas 3: vecteurisé
  P3 <- wass2s_class_probs_norm(mu = c(5, 15, 30), sigma = c(2, 2, 2), thresholds = th)
  s <- rowSums(as.matrix(P3))
  expect_true(all(abs(s - 1) < 1e-12))
  expect_true(all(P3$p_below >= 0 & P3$p_normal >= 0 & P3$p_above >= 0))
})

test_that("wass2s_class_entropy behaves as expected", {
  # Entropie nulle pour distribution certaine
  e0 <- wass2s_class_entropy(1, 0, 0)
  expect_equal(e0, 0, tolerance = 1e-12)

  # Entropie maximale pour distribution uniforme (log(3))
  eU <- wass2s_class_entropy(1/3, 1/3, 1/3)
  expect_equal(eU, log(3), tolerance = 1e-12)

  # Intermédiaire pour distribution non uniforme
  e1 <- wass2s_class_entropy(0.2, 0.5, 0.3)
  expect_true(e1 > 0 && e1 < log(3))
})

test_that("wass2s_class_from_forecast with RMSE returns coherent outputs", {
  # Climatologie + prévisions
  q_hist <- 100:199
  fused_final <- tibble::tibble(
    YYYY = 2018:2022,
    pred = c(120, 140, 180, 100, 175)
  )

  res <- wass2s_class_from_forecast(
    df     = fused_final,
    q_hist = q_hist,
    rmse   = 5  # sigma constant
  )

  expect_true(all(c("YYYY","pred","p_below","p_normal","p_above","class_hat","entropy") %in% names(res)))
  expect_equal(nrow(res), nrow(fused_final))
  # Ordre des années conservé
  expect_equal(res$YYYY, fused_final$YYYY)

  # Probabilités normalisées
  s <- res$p_below + res$p_normal + res$p_above
  expect_true(all(abs(s - 1) < 1e-10))

  # Classes cohérentes vs position de pred autour de Q1/Q3
  th <- wass2s_class_thr(q_hist)
  expect_true(all(ifelse(res$class_hat == "below", res$pred <= th[["t1"]], TRUE)))
  expect_true(all(ifelse(res$class_hat == "above", res$pred >= th[["t2"]], TRUE)))
})

test_that("wass2s_class_from_forecast accepts vector sigma and custom thresholds", {
  q_hist <- 100:199
  fused_final <- tibble::tibble(
    YYYY = 2000:2003,
    pred = c(110, 150, 170, 130)
  )
  # sigma vectorisé (croissant)
  sig <- c(2, 5, 10, 20)
  # Seuils personnalisés (plus serrés que Q1/Q3 pour le test)
  th   <- c(t1 = 120, t2 = 160)

  res <- wass2s_class_from_forecast(
    df         = fused_final,
    q_hist     = q_hist,
    sigma      = sig,
    thresholds = th
  )

  expect_equal(nrow(res), 4L)
  # Les proba doivent varier selon sigma : plus sigma est grand, plus la masse va sur les queues
  # Ici juste un contrôle de non-constance entre lignes 2 et 4
  expect_false(all(res$p_below[c(2,4)] == res$p_below[c(2,4)][1]))

  # Vérifie que les seuils personnalisés sont pris en compte :
  # pour pred=150 (au milieu), p_normal devrait être relativement élevée
  expect_true(res$p_normal[res$YYYY == 2001] > 0.4)
})

test_that("wass2s_class_from_forecast applies minimal sigma floor via min_sigma_frac when deriving sigma", {
  # Ici on ne passe ni sigma ni residuals et on met un rmse très petit
  # La fonction doit appliquer s <- max(rmse, min_sigma_frac * sd(q_hist))
  q_hist <- c(rep(1000, 10), rep(2000, 10))  # sd>0
  fused_final <- tibble::tibble(YYYY = 2020:2023, pred = c(900, 1100, 1500, 2100))

  # rmse ridiculement petit
  res <- wass2s_class_from_forecast(
    df     = fused_final,
    q_hist = q_hist,
    rmse   = 1e-9,         # => devrait être remplacé par s_min
    min_sigma_frac = 0.05
  )

  # On ne peut pas lire sigma interne, mais on vérifie au moins que les proba restent bien définies
  s <- res$p_below + res$p_normal + res$p_above
  expect_true(all(is.finite(s)))
  expect_true(all(abs(s - 1) < 1e-10))
})

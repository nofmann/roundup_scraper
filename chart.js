// Interactive plots + period selection for the Alta roundup. Inlined into roundup.html by roundup.R.
// D = {t: [epoch ms], <column>: [values|null], ...}; no dependencies.
// - Plots size to the container (1:1 pixels) and lay out in 1 or 2 columns.
// - Drag across any plot to select a period: plots zoom to it and the table shows just that period.
(function () {
  const TZ = "America/Denver", NS = "http://www.w3.org/2000/svg";
  const INK = "#222", MUTED = "#555", GRID = "#d9d9d9", NAVY = "#1C1370";
  const ELEV = [["8,550 ft", "8550", "#EF6323"], ["10,500 ft", "10500", "#00AE9B"], ["11,068 ft", "11068", "#0032A0"]];
  const GUT = 28, TOP = 32, GAP = 34, AXIS_H = 50;
  const ALL = window.ROUNDUP_DATA, host = document.getElementById("interactive");
  const DEFAULT_H = window.ROUNDUP_DEFAULT_HOURS || 72;
  let D, n;                 // the slice of ALL currently drawn
  let cur = null;           // {lo, hi} currently drawn

  let sel = null, prevRange = null;  // active period selection, and the range to return to
  const fin = (a) => a.filter((v) => v !== null);
  const fmtD = new Intl.DateTimeFormat("en-US", { timeZone: TZ, month: "short", day: "2-digit" });
  const fmtT = new Intl.DateTimeFormat("en-GB", { timeZone: TZ, hour: "2-digit", minute: "2-digit", hourCycle: "h23" });
  const fmtDay = new Intl.DateTimeFormat("en-US", { timeZone: TZ, weekday: "short", month: "short", day: "2-digit" });
  const hourOf = (t) => +fmtT.format(t).slice(0, 2) * 60 + +fmtT.format(t).slice(3);
  // Hour-ending convention: 00:00 is shown as 24:00 of the previous day.
  const dayLbl = (f, t) => f.format(t - 1000), timeLbl = (t) => fmtT.format(t).replace(/^00:/, "24:");
  const compass = (d) => ["N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW"][Math.round(d / 22.5) % 16];
  const niceRange = (lo, hi, k = 4) => {
    const raw = (hi - lo || 1) / k, p = Math.pow(10, Math.floor(Math.log10(raw)));
    const s = [1, 2, 5, 10].map((m) => m * p).find((x) => x >= raw);
    const a = Math.floor(lo / s) * s, b = Math.ceil(hi / s) * s, t = [];
    for (let v = a; v <= b + 1e-9; v += s) t.push(+v.toFixed(6));
    return { lo: a, hi: b, ticks: t };
  };

  function draw(lo, hi) {
  cur = { lo, hi };
  const keep = ALL.t.map((t) => t >= lo && t <= hi);
  D = {};
  for (const k in ALL) D[k] = ALL[k].filter((_, i) => keep[i]);
  n = D.t.length;
  host.textContent = "";
  if (n < 2) { host.textContent = "Fewer than two observations in this range."; return; }

  // ---- panel definitions ----
  const panels = [];
  const wy = niceRange(0, Math.max(0.05, ...fin(D.h2o_9664))), sy = niceRange(0, Math.max(2, ...fin(D.snow12_9664)));
  panels.push({
    title: "Precipitation at 9,664 ft", sub: "bars = hourly water equivalent (left) · line = rolling 12-hr snow (right)",
    ylab: "Water eq. (in)", ylabCol: "#0032A0", y: wy,
    y2: Object.assign({ lab: "12-hr snow (in)", col: "#EF6323" }, sy),
    layers: [
      { type: "bar", key: "h2o_9664", col: "#0032A0", label: "Water eq. 9,664 ft", unit: " in", dp: 2 },
      { type: "line", key: "snow12_9664", col: "#EF6323", label: "12-hr snow", unit: " in", dp: 1, sec: true, w: 2.2 },
    ],
  });
  const elevPanel = (title, ylab, prefix, extra) => {
    const vals = ELEV.flatMap((e) => fin(D[prefix + e[1]]));
    return Object.assign({
      title, ylab, y: niceRange(Math.min(...vals), Math.max(...vals)), legend: true,
      layers: ELEV.map((e) => ({ type: "line", key: prefix + e[1], col: e[2], label: e[0], dp: 0, w: 2 })),
    }, extra);
  };
  const tp = elevPanel("Temperature", "Temp (°F)", "temp_", { unit: "°F", sub: "hourly average" });
  tp.layers.forEach((l) => (l.unit = "°F"));
  tp.ref = 32; tp.refLabel = "32 °F";
  tp.y = niceRange(Math.min(32, tp.y.lo), Math.max(32, tp.y.hi));
  panels.push(tp);
  const rp = elevPanel("Relative humidity", "RH (%)", "rh_", { sub: "hourly average" });
  rp.layers.forEach((l) => (l.unit = "%"));
  rp.y.hi = Math.min(rp.y.hi, 100);
  rp.y.ticks = rp.y.ticks.filter((v) => v <= 100);
  panels.push(rp);
  [["8550", "8,550 ft", "#EF6323"], ["10500", "10,500 ft", "#00AE9B"], ["baldy", "Mt. Baldy", "#555555"]].forEach(([s, label, col]) => {
    const spd = D["wspd_" + s], gust = D["wgust_" + s];
    if (!spd || !spd.concat(gust).some((v) => v > 0)) return;
    const wyr = niceRange(0, Math.max(5, ...fin(spd), ...fin(gust))), ymax = wyr.hi;
    panels.push({
      title: "Wind at " + label, sub: "solid = average · dashed = max gust · circles = direction (right)",
      ylab: "Speed (mph)", y: wyr,
      y2: { lab: "Direction", col: INK, lo: 0, hi: 360, ticks: [0, 90, 180, 270, 360], names: ["N", "E", "S", "W", "N"] },
      layers: [
        { type: "line", key: "wgust_" + s, col, label: "Gust", unit: " mph", dp: 0, w: 1.4, dash: "5 4" },
        { type: "line", key: "wspd_" + s, col, label: "Avg", unit: " mph", dp: 0, w: 2.4 },
        { type: "dir", key: "wdir_" + s, spd: "wspd_" + s, label: "Dir", sec: true, col },
      ],
    });
  });

  // ---- geometry: width follows the container; 1 or 2 columns ----
  const Wtot = 1200;  // fixed drawing width; the SVG scales to the page
  const cols = 1;
  const CW = cols === 2 ? (Wtot - GUT) / 2 : Wtot;
  const small = false, ML = 70, MR = 70;
  const PH = 150;
  const showSub = CW >= 640;
  const rows = Math.ceil(panels.length / cols);
  const H = TOP + rows * PH + (rows - 1) * GAP + AXIS_H;
  panels.forEach((p, i) => { p.col = i % cols; p.row = Math.floor(i / cols); p.ox = p.col * (CW + GUT); p.top = TOP + p.row * (PH + GAP); });
  const colAxisY = [];  // y of each column's bottom axis
  panels.forEach((p) => (colAxisY[p.col] = p.top + PH));
  const t0 = D.t[0], t1 = D.t[n - 1], pad = (t1 - t0) * 0.01;
  const X = (t) => ML + ((t - (t0 - pad)) / (t1 - t0 + 2 * pad)) * (CW - ML - MR);  // within a column
  const Y = (p, v, sec) => { const a = sec ? p.y2 : p.y; v = Math.max(a.lo, Math.min(a.hi, v)); return p.top + PH - ((v - a.lo) / (a.hi - a.lo)) * PH; };  // clamp so outliers stay in the frame

  const el = (name, attrs, parent, text) => {
    const e = document.createElementNS(NS, name);
    for (const k in attrs) e.setAttribute(k, attrs[k]);
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  };
  const svg = el("svg", { viewBox: `0 0 ${Wtot} ${H}`, class: "ichart", role: "img", "aria-label": "Weather plots" });
  host.appendChild(svg);
  const style = (e, s) => { e.setAttribute("style", s); return e; };

  // x ticks: spacing follows the span (local Mountain time)
  const spanH = (t1 - t0) / 3.6e6, mult = small ? 2 : 1;
  const stepMin = (spanH > 48 ? 12 : spanH > 24 ? 6 : spanH > 12 ? 3 : 1) * mult * 60;
  let xticks;
  if (spanH <= 96) xticks = D.t.filter((t) => hourOf(t) % stepMin === 0);
  else {  // long ranges: midnights only, thinned to about a dozen labels
    const mids = D.t.filter((t) => hourOf(t) === 0), k = Math.ceil(mids.length / (small ? 6 : 12));
    xticks = mids.filter((_, i) => i % k === 0);
  }

  panels.forEach((p) => {
    const g = el("g", {}, svg), x0 = p.ox + ML, x1 = p.ox + CW - MR, px = (t) => p.ox + X(t);
    xticks.forEach((t) => el("line", { x1: px(t), x2: px(t), y1: p.top, y2: p.top + PH, stroke: GRID, "stroke-width": 0.7 }, g));
    p.y.ticks.forEach((v) => {
      el("line", { x1: x0, x2: x1, y1: Y(p, v), y2: Y(p, v), stroke: GRID, "stroke-width": 0.7 }, g);
      el("text", { x: x0 - 6, y: Y(p, v) + 4, "text-anchor": "end", "font-size": 13, fill: INK }, g, +v.toFixed(2) + (p.ylab.startsWith("RH") ? "%" : ""));
    });
    if (p.ref !== undefined) {
      el("line", { x1: x0, x2: x1, y1: Y(p, p.ref), y2: Y(p, p.ref), stroke: MUTED, "stroke-dasharray": "5 4" }, g);
      el("text", { x: x0 + 4, y: Y(p, p.ref) - 4, "font-size": 13, fill: MUTED }, g, p.refLabel);
    }
    if (p.y2) p.y2.ticks.forEach((v, k) =>
      el("text", { x: x1 + 6, y: Y(p, v, true) + 4, "font-size": 13, fill: INK }, g, p.y2.names ? p.y2.names[k] : +v.toFixed(2)));

    // data layers
    p.layers.forEach((l) => {
      const v = D[l.key]; if (!v) return;
      if (l.type === "bar") {
        const bw = ((CW - ML - MR) / (t1 - t0 + 2 * pad)) * 2400 * 1000 * 0.9;
        v.forEach((y, k) => { if (y === null || y <= 0) return;
          el("rect", { x: px(D.t[k]) - bw / 2, width: bw, y: Y(p, y), height: p.top + PH - Y(p, y), fill: l.col }, g); });
      } else if (l.type === "line") {
        let d = "", pen = false;
        v.forEach((y, k) => { if (y === null) { pen = false; return; }
          d += (pen ? "L" : "M") + px(D.t[k]).toFixed(1) + " " + Y(p, y, l.sec).toFixed(1); pen = true; });
        el("path", { d, fill: "none", stroke: l.col, "stroke-width": l.w, "stroke-dasharray": l.dash || "", "stroke-linejoin": "round" }, g);
      } else {
        v.forEach((y, k) => { if (y === null || D[l.spd][k] === 0 || D[l.spd][k] === null) return;
          el("circle", { cx: px(D.t[k]), cy: Y(p, y / 360 * p.y2.hi, true), r: 3, fill: l.col, stroke: INK, "stroke-width": 1 }, g); });
      }
    });

    // frame, title, axis titles
    el("rect", { x: x0, y: p.top, width: x1 - x0, height: PH, fill: "none", stroke: INK, "stroke-width": 1 }, g);
    const ti = el("text", { x: p.ox, y: p.top - 12, "font-size": 15, "font-weight": 700, fill: NAVY }, g, p.title);
    if (showSub && p.sub) el("tspan", { "font-size": 12, "font-weight": 400, fill: MUTED, dx: 10 }, ti, p.sub);
    el("text", { transform: `translate(${p.ox + 14} ${p.top + PH / 2}) rotate(-90)`, "text-anchor": "middle", "font-size": 14, fill: p.ylabCol || INK, "font-weight": p.ylabCol ? 700 : 400 }, g, p.ylab);
    if (p.y2) el("text", { transform: `translate(${p.ox + CW - 12} ${p.top + PH / 2}) rotate(90)`, "text-anchor": "middle", "font-size": 14, fill: p.y2.col, "font-weight": p.y2.col !== INK ? 700 : 400 }, g, p.y2.lab);
    if (p.legend) { let lx = x1; [...ELEV].reverse().forEach((e) => {
      const lab = el("text", { x: lx - 4, y: p.top - 12, "text-anchor": "end", "font-size": 14, fill: INK }, g, e[0]);
      const w = lab.getComputedTextLength ? lab.getComputedTextLength() || 62 : 62;
      el("line", { x1: lx - w - 24, x2: lx - w - 8, y1: p.top - 15, y2: p.top - 15, stroke: e[2], "stroke-width": 3 }, g); lx -= w + 40; }); }
  });
  // bottom time axis, one per column
  for (let c = 0; c < cols; c++) {
    if (colAxisY[c] === undefined) continue;
    const ay = colAxisY[c], ox = c * (CW + GUT);
    xticks.forEach((t) => {
      el("line", { x1: ox + X(t), x2: ox + X(t), y1: ay, y2: ay + 5, stroke: INK }, svg);
      el("text", { x: ox + X(t), y: ay + 20, "text-anchor": "middle", "font-size": 13, fill: INK }, svg, dayLbl(fmtD, t));
      el("text", { x: ox + X(t), y: ay + 37, "text-anchor": "middle", "font-size": 13, fill: INK }, svg, timeLbl(t));
    });
  }

  // ---- selection band (drag) + hover crosshair across all panels ----
  const bands = [], crosses = [];
  for (let c = 0; c < cols; c++) if (colAxisY[c] !== undefined) {
    bands[c] = style(el("rect", { y: TOP, height: colAxisY[c] - TOP, fill: NAVY, "fill-opacity": 0.14, stroke: NAVY, "stroke-width": 1 }, svg), "display:none;pointer-events:none");
    crosses[c] = style(el("line", { y1: TOP, y2: colAxisY[c], stroke: INK, "stroke-width": 1, "stroke-dasharray": "3 3" }, svg), "display:none;pointer-events:none");
  }
  const dots = [];
  panels.forEach((p) => p.layers.forEach((l) => {
    if (l.type === "bar") return;
    dots.push({ p, l, c: style(el("circle", { r: 4, fill: l.col || "#fff", stroke: "#fff", "stroke-width": 1.5 }, svg), "display:none;pointer-events:none") });
  }));
  const tip = document.createElement("div"); tip.className = "itip"; host.appendChild(tip);
  const val = (l, k) => { const v = D[l.key][k]; return v === null ? "–" : l.type === "dir" ? (D[l.spd][k] ? compass(v) + " " + String(Math.round(v)).padStart(3, "0") + "°" : "calm") : v.toFixed(l.dp) + (l.unit || ""); };
  const row = (c, name, v) => `<tr><td><i style="background:${c}"></i>${name}</td><td>${v}</td></tr>`;

  function show(k, clientX, clientY) {
    const t = D.t[k];
    crosses.forEach((cr, c) => { if (!cr) return; const x = c * (CW + GUT) + X(t); cr.setAttribute("x1", x); cr.setAttribute("x2", x); cr.style.display = ""; });
    dots.forEach((d) => { const v = D[d.l.key][k];
      const ok = v !== null && !(d.l.type === "dir" && !D[d.l.spd][k]);
      d.c.style.display = ok ? "" : "none";
      if (ok) { d.c.setAttribute("cx", d.p.ox + X(t)); d.c.setAttribute("cy", Y(d.p, d.l.type === "dir" ? v / 360 * d.p.y2.hi : v, d.l.sec));
        if (d.l.type === "dir") d.c.setAttribute("stroke", INK); } });
    const sec = (title, rows) => `<tr class="h"><th colspan="2">${title}</th></tr>` + rows;
    let h = `<div class="tt">${dayLbl(fmtDay, t)}, ${timeLbl(t)} <span>(hour ending)</span></div><table>`;
    h += sec("Precip at 9,664 ft", row("#0032A0", "Water eq.", val({ key: "h2o_9664", dp: 2, unit: " in" }, k)) + row("#EF6323", "12-hr snow", val({ key: "snow12_9664", dp: 1, unit: " in" }, k)));
    h += sec("Temp", ELEV.map((e) => row(e[2], e[0], val({ key: "temp_" + e[1], dp: 0, unit: "°F" }, k))).join(""));
    h += sec("RH", ELEV.map((e) => row(e[2], e[0], val({ key: "rh_" + e[1], dp: 0, unit: "%" }, k))).join(""));
    [["8550", "8,550 ft", "#EF6323"], ["10500", "10,500 ft", "#00AE9B"], ["baldy", "Mt. Baldy", "#555555"]].forEach(([s, label, col]) => {
      if (!panels.some((p) => p.layers.some((l) => l.key === "wspd_" + s))) return;
      h += sec("Wind " + label, row(col, "Avg / gust", val({ key: "wspd_" + s, dp: 0, unit: "" }, k) + " / " + val({ key: "wgust_" + s, dp: 0, unit: " mph" }, k)) +
        row(col, "Direction", val({ key: "wdir_" + s, spd: "wspd_" + s, type: "dir" }, k)));
    });
    tip.innerHTML = h + "</table>"; tip.style.display = "block";
    const hr = host.getBoundingClientRect(), tw = tip.offsetWidth, th = tip.offsetHeight;
    let lx = clientX - hr.left + 16; if (lx + tw > hr.width) lx = clientX - hr.left - tw - 16;
    let ly = clientY - hr.top - th / 2; ly = Math.max(0, Math.min(ly, hr.height - th));
    tip.style.left = lx + "px"; tip.style.top = ly + "px";
  }
  function hide() { crosses.forEach((c) => c && (c.style.display = "none")); tip.style.display = "none"; dots.forEach((d) => (d.c.style.display = "none")); }

  // pointer helpers
  const toLoc = (e) => { const pt = svg.createSVGPoint(); pt.x = e.clientX; pt.y = e.clientY; return pt.matrixTransform(svg.getScreenCTM().inverse()); };
  const colAt = (x) => (cols === 2 && x > CW + GUT / 2 ? 1 : 0);
  const clampX = (x, c) => { const ox = c * (CW + GUT); return Math.max(ox + ML, Math.min(ox + CW - MR, x)); };
  const nearest = (c, x) => { let best = 0, bd = Infinity; const ox = c * (CW + GUT);
    D.t.forEach((t, k) => { const d = Math.abs(ox + X(t) - x); if (d < bd) { bd = d; best = k; } }); return best; };
  const setBands = (c, ka, kb) => { const a = Math.min(ka, kb), b = Math.max(ka, kb);
    bands.forEach((bd, cc) => { if (!bd) return; const ox = cc * (CW + GUT);
      bd.setAttribute("x", ox + X(D.t[a])); bd.setAttribute("width", Math.max(1, X(D.t[b]) - X(D.t[a]))); bd.style.display = ""; }); };

  let drag = null;
  svg.addEventListener("pointerdown", (e) => {
    if (e.button !== undefined && e.button !== 0) return;
    const l = toLoc(e), c = colAt(l.x), ox = c * (CW + GUT);
    if (colAxisY[c] === undefined || l.x < ox + ML || l.x > ox + CW - MR || l.y < TOP || l.y > colAxisY[c]) return;
    drag = { c, k0: nearest(c, l.x), k1: nearest(c, l.x), id: e.pointerId };
    try { svg.setPointerCapture(e.pointerId); } catch (_) {}
    hide();
  });
  svg.addEventListener("pointermove", (e) => {
    const l = toLoc(e);
    if (drag) { drag.k1 = nearest(drag.c, clampX(l.x, drag.c)); if (drag.k1 !== drag.k0) setBands(drag.c, drag.k0, drag.k1); return; }
    const c = colAt(l.x), ox = c * (CW + GUT);
    if (l.x < ox + ML || l.x > ox + CW - MR) return hide();
    show(nearest(c, l.x), e.clientX, e.clientY);
  });
  const endDrag = (e, ok) => {
    if (!drag) return;
    const d = drag; drag = null;
    try { svg.releasePointerCapture(d.id); } catch (_) {}
    bands.forEach((b) => b && (b.style.display = "none"));
    if (ok && d.k0 !== d.k1) onSelect(D.t[Math.min(d.k0, d.k1)], D.t[Math.max(d.k0, d.k1)]);
  };
  svg.addEventListener("pointerup", (e) => endDrag(e, true));
  svg.addEventListener("pointercancel", (e) => endDrag(e, false));
  svg.addEventListener("pointerleave", () => { if (!drag) hide(); });
  svg.addEventListener("dblclick", () => { if (sel) clearSel(); });
  }

  // ---- table for a selected period (same look as the server-rendered one) ----
  const tbl = document.querySelector("table.wx"), tbody = tbl && tbl.tBodies[0];
  const tTitle = document.getElementById("tbl-title"), tWrap = document.getElementById("tbl-wrap");
  const origBody = tbody ? tbody.innerHTML : "", origTitle = tTitle ? tTitle.innerHTML : "";
  const lin = (c) => { c /= 255; return c <= 0.04045 ? c / 12.92 : Math.pow((c + 0.055) / 1.055, 2.4); };
  const unlin = (c) => 255 * (c <= 0.0031308 ? 12.92 * c : 1.055 * Math.pow(c, 1 / 2.4) - 0.055);
  const WP = [0.95047, 1, 1.08883], EPS = 216 / 24389, KAP = 24389 / 27;
  const lf = (t) => (t > EPS ? Math.cbrt(t) : (KAP * t + 16) / 116), lfi = (t) => (t * t * t > EPS ? t * t * t : (116 * t - 16) / KAP);
  const toLab = (hex) => {
    const [r, g, b] = [1, 3, 5].map((i) => lin(parseInt(hex.substr(i, 2), 16)));
    const x = 0.4124564 * r + 0.3575761 * g + 0.1804375 * b, y = 0.2126729 * r + 0.7151522 * g + 0.0721750 * b, z = 0.0193339 * r + 0.1191920 * g + 0.9503041 * b;
    const fx = lf(x / WP[0]), fy = lf(y / WP[1]), fz = lf(z / WP[2]);
    return [116 * fy - 16, 500 * (fx - fy), 200 * (fy - fz)];
  };
  const fromLab = ([L, a, b]) => {
    const fy = (L + 16) / 116, fx = fy + a / 500, fz = fy - b / 200;
    const x = lfi(fx) * WP[0], y = lfi(fy) * WP[1], z = lfi(fz) * WP[2];
    const rgb = [3.2404542 * x - 1.5371385 * y - 0.4985314 * z, -0.9692660 * x + 1.8760108 * y + 0.0415560 * z, 0.0556434 * x - 0.2040259 * y + 1.0572252 * z];
    return rgb.map((c) => Math.max(0, Math.min(255, Math.round(unlin(Math.max(0, Math.min(1, c)))))));
  };
  const ramp = (cols) => { const labs = cols.map(toLab); return (t) => { const s = t * (labs.length - 1), i = Math.min(labs.length - 2, Math.floor(s)), f = s - i;
    return fromLab(labs[i].map((v, j) => v + (labs[i + 1][j] - v) * f)); }; };
  const NA = '<td class="na">&ndash;</td>';
  const cellFn = (lo, hi, cols, dp) => { const r = ramp(cols); return (x) => {
    if (x === null || x === undefined) return NA;
    const t = Math.min(Math.max((x - lo) / (hi - lo), 0), 1), c = r(t);
    const lum = (0.299 * c[0] + 0.587 * c[1] + 0.114 * c[2]) / 255;
    return `<td style="background:rgb(${c[0]},${c[1]},${c[2]});color:${lum < 0.5 ? "#fff" : "#111"}">${x.toFixed(dp)}</td>`; }; };
  const precipC = cellFn(0, 0.15, ["#ffffff", "#2b7bba"], 2), snowC = cellFn(0, 12, ["#ffffff", "#2b7bba"], 1);
  const tempC = cellFn(2, 62, ["#2f5d9e", "#f4f4f4", "#d1495b"], 0), rhC = cellFn(40, 100, ["#ffffff", "#7fb3d5"], 0);
  const windC = cellFn(0, 40, ["#ffffff", "#f6a04d", "#b8382c"], 0);
  const dirC = (spd, deg) => { if (deg === null || deg === undefined || spd === null || spd === undefined || spd === 0) return NA;
    const d = ((Math.round(deg) % 360) + 360) % 360;
    return `<td title='${String(d).padStart(3, "0")}&deg;'>${compass(deg)} <svg class='vec' viewBox='-8 -8 16 16' style='transform:rotate(${d}deg)'><path d='M0-6V6M-4 2L0 6L4 2' fill='none' stroke='currentColor' stroke-width='1.8' stroke-linecap='round' stroke-linejoin='round'/></svg></td>`; };
  const at = (key, i) => { const a = ALL[key]; return a ? a[i] : null; };
  const dayText = (t) => dayLbl(fmtDay, t).replace(",", "");

  function renderTable(lo, hi) {
    if (!tbody) return;
    const idx = []; ALL.t.forEach((t, i) => { if (t >= lo && t <= hi) idx.push(i); });
    let html = "", lastDay = null;
    for (let j = idx.length - 1; j >= 0; j--) {
      const i = idx[j], t = ALL.t[i], day = dayText(t);
      if (day !== lastDay) { html += `<tr class='day'><td colspan='18'>${day}</td></tr>`; lastDay = day; }
      html += "<tr><td class='dt'>" + timeLbl(t) + "</td>" + precipC(at("h2o_9664", i)) + snowC(at("snow12_9664", i)) +
        ["8550", "10500", "11068"].map((s) => tempC(at("temp_" + s, i))).join("") +
        ["8550", "10500", "11068"].map((s) => rhC(at("rh_" + s, i))).join("") +
        ["8550", "10500", "baldy"].map((s) => windC(at("wspd_" + s, i)) + dirC(at("wspd_" + s, i), at("wdir_" + s, i)) + windC(at("wgust_" + s, i))).join("") + "</tr>";
    }
    tbody.innerHTML = html;
    if (tTitle) tTitle.textContent = `Selected period: ${dayText(lo)} ${timeLbl(lo)} to ${dayText(hi)} ${timeLbl(hi)} (${idx.length} hours)`;
    if (tWrap) { tWrap.classList.add("tall"); tWrap.scrollTop = 0; }
  }
  function restoreTable() {
    if (!tbody) return;
    tbody.innerHTML = origBody;
    if (tTitle) tTitle.innerHTML = origTitle;
    if (tWrap) tWrap.classList.remove("tall");
  }

  // ---- date/time range controls (Mountain time) ----
  const fmtWall = new Intl.DateTimeFormat("en-CA", { timeZone: TZ, year: "numeric", month: "2-digit", day: "2-digit", hour: "2-digit", minute: "2-digit", hourCycle: "h23" });
  const wallParts = (t) => { const p = {}; fmtWall.formatToParts(t).forEach((x) => (p[x.type] = +x.value)); return Date.UTC(p.year, p.month - 1, p.day, p.hour, p.minute); };
  const toWall = (t) => new Date(wallParts(t)).toISOString().slice(0, 16);
  const fromWall = (s) => { const u = Date.parse(s + "Z"); let t = u; for (let i = 0; i < 2; i++) t += u - wallParts(t); return t; };

  const tMin = ALL.t[0], tMax = ALL.t[ALL.t.length - 1];
  const bar = document.createElement("div"); bar.className = "rangebar";
  bar.innerHTML = '<label>From <input type="datetime-local" id="rfrom"></label><label>To <input type="datetime-local" id="rto"></label>' +
    '<span class="presets"></span>' +
    '<button type="button" id="rclr" hidden>&#8617; Clear selection</button><a id="rjump" href="#tbl-title" hidden>View in table &uarr;</a>' +
    '<span class="note">Mountain time, hour ending &middot; drag across any plot to zoom to that period and filter the table</span>';
  host.parentNode.insertBefore(bar, host);
  const inFrom = bar.querySelector("#rfrom"), inTo = bar.querySelector("#rto");
  const btnClr = bar.querySelector("#rclr"), lnkJump = bar.querySelector("#rjump");
  [inFrom, inTo].forEach((i) => { i.min = toWall(tMin); i.max = toWall(tMax); });

  const go = (lo, hi) => {
    lo = Math.max(tMin, lo); hi = Math.min(tMax, hi);
    if (lo > hi) [lo, hi] = [hi, lo];
    inFrom.value = toWall(lo); inTo.value = toWall(hi);
    draw(lo, hi);
    return [lo, hi];
  };
  function endSel() { sel = null; prevRange = null; restoreTable(); btnClr.hidden = true; lnkJump.hidden = true; }
  function clearSel() { const p = prevRange; endSel(); if (p) go(p.lo, p.hi); }
  function onSelect(lo, hi) {
    if (!sel) prevRange = cur;
    sel = { lo, hi };
    [lo, hi] = go(lo, hi);
    renderTable(lo, hi);
    btnClr.hidden = false; lnkJump.hidden = false;
  }
  const apply = (lo, hi) => { if (sel) endSel(); go(lo, hi); };
  btnClr.addEventListener("click", clearSel);

  [["24 h", 24], ["72 h", 72], ["7 d", 168], ["30 d", 720], ["All", Infinity]].forEach(([label, h]) => {
    const b = document.createElement("button"); b.type = "button"; b.textContent = label;
    b.addEventListener("click", () => apply(tMax - h * 3.6e6, tMax));
    bar.querySelector(".presets").appendChild(b);
  });
  const fromInputs = () => { if (inFrom.value && inTo.value) apply(fromWall(inFrom.value), fromWall(inTo.value)); };
  inFrom.addEventListener("change", fromInputs); inTo.addEventListener("change", fromInputs);

  go(tMax - DEFAULT_H * 3.6e6, tMax);
})();

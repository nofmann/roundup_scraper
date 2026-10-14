// Interactive stacked plots for the Alta roundup. Inlined into roundup.html by roundup.R.
// D = {t: [epoch ms], <column>: [values|null], ...}; no dependencies.
(function () {
  const TZ = "America/Denver", NS = "http://www.w3.org/2000/svg";
  const INK = "#222", MUTED = "#555", GRID = "#d9d9d9";
  const ELEV = [["8,550 ft", "8550", "#EF6323"], ["10,500 ft", "10500", "#00AE9B"], ["11,068 ft", "11068", "#0032A0"]];
  const W = 1200, ML = 64, MR = 64, PH = 150, GAP = 30, TOP = 26, AXIS_H = 44;
  const D = window.ROUNDUP_DATA, host = document.getElementById("interactive");
  const n = D.t.length;
  const has = (k) => D[k] && D[k].some((v) => v !== null);
  const fin = (a) => a.filter((v) => v !== null);
  const fmtD = new Intl.DateTimeFormat("en-US", { timeZone: TZ, month: "short", day: "2-digit" });
  const fmtT = new Intl.DateTimeFormat("en-GB", { timeZone: TZ, hour: "2-digit", minute: "2-digit", hourCycle: "h23" });
  const fmtDay = new Intl.DateTimeFormat("en-US", { timeZone: TZ, weekday: "short", month: "short", day: "2-digit" });
  const hourOf = (t) => +fmtT.format(t).slice(0, 2) * 60 + +fmtT.format(t).slice(3);
  const compass = (d) => ["N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW"][Math.round(d / 22.5) % 16];
  const niceMax = (v) => { const p = Math.pow(10, Math.floor(Math.log10(v))); for (const m of [1, 2, 2.5, 5, 10]) if (m * p >= v) return m * p; };
  const ticksTo = (max, k) => { const s = niceMax(max / k) / 1; return Array.from({ length: Math.floor(max / s + 1e-9) + 1 }, (_, i) => i * s); };
  const niceRange = (lo, hi, k = 4) => {
    const raw = (hi - lo || 1) / k, p = Math.pow(10, Math.floor(Math.log10(raw)));
    const s = [1, 2, 5, 10].map((m) => m * p).find((x) => x >= raw);
    const a = Math.floor(lo / s) * s, b = Math.ceil(hi / s) * s, t = [];
    for (let v = a; v <= b + 1e-9; v += s) t.push(+v.toFixed(6));
    return { lo: a, hi: b, ticks: t };
  };

  // ---- panel definitions ----
  const panels = [];
  const wy = niceRange(0, Math.max(0.05, ...fin(D.h2o_9664))), sy = niceRange(0, Math.max(2, ...fin(D.snow12_9664)));
  panels.push({
    title: "Precipitation at 9,664 ft: bars = hourly water equivalent (left), line = rolling 12-hr snow (right)",
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
  const tp = elevPanel("Temperature by elevation (hourly average)", "Temp (°F)", "temp_", { unit: "°F" });
  tp.layers.forEach((l) => (l.unit = "°F"));
  tp.ref = 32; tp.refLabel = "32 °F";
  tp.y = niceRange(Math.min(32, tp.y.lo), Math.max(32, tp.y.hi));
  panels.push(tp);
  const rp = elevPanel("Relative humidity by elevation (hourly average)", "RH (%)", "rh_", {});
  rp.layers.forEach((l) => (l.unit = "%"));
  rp.y.hi = Math.min(rp.y.hi, 100);
  rp.y.ticks = rp.y.ticks.filter((v) => v <= 100);
  panels.push(rp);
  [["8550", "8,550 ft", "#EF6323"], ["10500", "10,500 ft", "#00AE9B"], ["baldy", "Mt. Baldy", "#555555"]].forEach(([s, label, col]) => {
    const spd = D["wspd_" + s], gust = D["wgust_" + s];
    if (!spd || !spd.concat(gust).some((v) => v > 0)) return;
    const wyr = niceRange(0, Math.max(5, ...fin(spd), ...fin(gust))), ymax = wyr.hi;
    panels.push({
      title: "Wind at " + label + ": solid = average, dashed = max gust, circles = direction (right)",
      ylab: "Speed (mph)", y: wyr,
      y2: { lab: "Direction", col: INK, lo: 0, hi: 360, ticks: [0, 90, 180, 270, 360], names: ["N", "E", "S", "W", "N"] },
      layers: [
        { type: "line", key: "wgust_" + s, col, label: "Gust", unit: " mph", dp: 0, w: 1.4, dash: "5 4" },
        { type: "line", key: "wspd_" + s, col, label: "Avg", unit: " mph", dp: 0, w: 2.4 },
        { type: "dir", key: "wdir_" + s, spd: "wspd_" + s, label: "Dir", sec: true },
      ],
    });
  });

  // ---- geometry ----
  const t0 = D.t[0], t1 = D.t[n - 1], pad = (t1 - t0) * 0.01;
  const X = (t) => ML + ((t - (t0 - pad)) / (t1 - t0 + 2 * pad)) * (W - ML - MR);
  const H = TOP + panels.length * PH + (panels.length - 1) * GAP + AXIS_H;
  panels.forEach((p, i) => (p.top = TOP + i * (PH + GAP)));
  const Y = (p, v, sec) => { const a = sec ? p.y2 : p.y; return p.top + PH - ((v - a.lo) / (a.hi - a.lo)) * PH; };

  const el = (name, attrs, parent, text) => {
    const e = document.createElementNS(NS, name);
    for (const k in attrs) e.setAttribute(k, attrs[k]);
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  };
  const svg = el("svg", { viewBox: `0 0 ${W} ${H}`, class: "ichart", role: "img", "aria-label": "Stacked weather plots" });
  host.appendChild(svg);
  const style = (e, s) => { e.setAttribute("style", s); return e; };

  // x ticks: every 12 / 6 / 3 hours depending on span (local Mountain time)
  const spanH = (t1 - t0) / 3.6e6, stepMin = (spanH > 48 ? 12 : spanH > 24 ? 6 : 3) * 60;
  const xticks = D.t.filter((t) => hourOf(t) % stepMin === 0);

  panels.forEach((p, i) => {
    const g = el("g", {}, svg), x0 = ML, x1 = W - MR;
    xticks.forEach((t) => el("line", { x1: X(t), x2: X(t), y1: p.top, y2: p.top + PH, stroke: GRID, "stroke-width": 0.7 }, g));
    p.y.ticks.forEach((v) => {
      el("line", { x1: x0, x2: x1, y1: Y(p, v), y2: Y(p, v), stroke: GRID, "stroke-width": 0.7 }, g);
      el("text", { x: x0 - 6, y: Y(p, v) + 4, "text-anchor": "end", "font-size": 11, fill: INK }, g, +v.toFixed(2) + (p.ylab.startsWith("RH") ? "%" : ""));
    });
    if (p.ref !== undefined) {
      el("line", { x1: x0, x2: x1, y1: Y(p, p.ref), y2: Y(p, p.ref), stroke: MUTED, "stroke-dasharray": "5 4" }, g);
      el("text", { x: x0 + 4, y: Y(p, p.ref) - 4, "font-size": 11, fill: MUTED }, g, p.refLabel);
    }
    if (p.y2) p.y2.ticks.forEach((v, k) =>
      el("text", { x: x1 + 6, y: Y(p, v, true) + 4, "font-size": 11, fill: INK }, g, p.y2.names ? p.y2.names[k] : +v.toFixed(2)));

    // data layers
    p.layers.forEach((l) => {
      const v = D[l.key]; if (!v) return;
      if (l.type === "bar") {
        const bw = ((W - ML - MR) / (t1 - t0 + 2 * pad)) * 2400 * 1000 * 0.9;
        v.forEach((y, k) => { if (y === null || y <= 0) return;
          el("rect", { x: X(D.t[k]) - bw / 2, width: bw, y: Y(p, y), height: p.top + PH - Y(p, y), fill: l.col }, g); });
      } else if (l.type === "line") {
        let d = "", pen = false;
        v.forEach((y, k) => { if (y === null) { pen = false; return; }
          d += (pen ? "L" : "M") + X(D.t[k]).toFixed(1) + " " + Y(p, y, l.sec).toFixed(1); pen = true; });
        el("path", { d, fill: "none", stroke: l.col, "stroke-width": l.w, "stroke-dasharray": l.dash || "", "stroke-linejoin": "round" }, g);
      } else {
        v.forEach((y, k) => { if (y === null || D[l.spd][k] === 0 || D[l.spd][k] === null) return;
          el("circle", { cx: X(D.t[k]), cy: Y(p, y / 360 * p.y2.hi, true), r: 3, fill: "#fff", stroke: INK, "stroke-width": 1 }, g); });
      }
    });

    // frame, title, axis titles
    el("rect", { x: x0, y: p.top, width: x1 - x0, height: PH, fill: "none", stroke: INK, "stroke-width": 1 }, g);
    el("text", { x: 0, y: p.top - 9, "font-size": 13, "font-weight": 700, fill: INK }, g, p.title);
    const yl = el("text", { transform: `translate(14 ${p.top + PH / 2}) rotate(-90)`, "text-anchor": "middle", "font-size": 12, fill: p.ylabCol || INK, "font-weight": p.ylabCol ? 700 : 400 }, g, p.ylab);
    if (p.y2) el("text", { transform: `translate(${W - 12} ${p.top + PH / 2}) rotate(90)`, "text-anchor": "middle", "font-size": 12, fill: p.y2.col, "font-weight": p.y2.col !== INK ? 700 : 400 }, g, p.y2.lab);
    if (p.legend) { let lx = x1; [...ELEV].reverse().forEach((e) => {
      const lab = el("text", { x: lx - 4, y: p.top - 9, "text-anchor": "end", "font-size": 12, fill: INK }, g, e[0]);
      const w = lab.getComputedTextLength ? lab.getComputedTextLength() || 62 : 62;
      el("line", { x1: lx - w - 24, x2: lx - w - 8, y1: p.top - 13, y2: p.top - 13, stroke: e[2], "stroke-width": 3 }, g); lx -= w + 40; }); }
  });
  // bottom time axis
  const ay = panels[panels.length - 1].top + PH;
  xticks.forEach((t) => {
    el("line", { x1: X(t), x2: X(t), y1: ay, y2: ay + 5, stroke: INK }, svg);
    el("text", { x: X(t), y: ay + 18, "text-anchor": "middle", "font-size": 11, fill: INK }, svg, fmtD.format(t));
    el("text", { x: X(t), y: ay + 32, "text-anchor": "middle", "font-size": 11, fill: INK }, svg, fmtT.format(t));
  });

  // ---- hover: crosshair across all panels + tooltip ----
  const cross = style(el("line", { y1: TOP, y2: ay, stroke: INK, "stroke-width": 1, "stroke-dasharray": "3 3" }, svg), "display:none;pointer-events:none");
  const dots = [];
  panels.forEach((p) => p.layers.forEach((l) => {
    if (l.type === "bar") return;
    dots.push({ p, l, c: style(el("circle", { r: 4, fill: l.col || "#fff", stroke: "#fff", "stroke-width": 1.5 }, svg), "display:none;pointer-events:none") });
  }));
  const tip = document.createElement("div"); tip.className = "itip"; host.appendChild(tip);
  const val = (l, k) => { const v = D[l.key][k]; return v === null ? "–" : l.type === "dir" ? (D[l.spd][k] ? compass(v) + " " + String(Math.round(v)).padStart(3, "0") + "°" : "calm") : v.toFixed(l.dp) + (l.unit || ""); };
  const row = (c, name, v) => `<tr><td><i style="background:${c}"></i>${name}</td><td>${v}</td></tr>`;

  function show(k, clientX, clientY) {
    const t = D.t[k], x = X(t);
    cross.setAttribute("x1", x); cross.setAttribute("x2", x); cross.style.display = "";
    dots.forEach((d) => { const v = D[d.l.key][k];
      const ok = v !== null && !(d.l.type === "dir" && !D[d.l.spd][k]);
      d.c.style.display = ok ? "" : "none";
      if (ok) { d.c.setAttribute("cx", x); d.c.setAttribute("cy", Y(d.p, d.l.type === "dir" ? v / 360 * d.p.y2.hi : v, d.l.sec));
        if (d.l.type === "dir") d.c.setAttribute("stroke", INK); } });
    const wl = (key, s) => panels.flatMap((p) => p.layers).find((l) => l.key === key + s);
    const sec = (title, rows) => `<tr class="h"><th colspan="2">${title}</th></tr>` + rows;
    let h = `<div class="tt">${fmtDay.format(t)}, ${fmtT.format(t)} <span>(hour ending)</span></div><table>`;
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
  function hide() { cross.style.display = "none"; tip.style.display = "none"; dots.forEach((d) => (d.c.style.display = "none")); }
  svg.addEventListener("pointermove", (e) => {
    const pt = svg.createSVGPoint(); pt.x = e.clientX; pt.y = e.clientY;
    const loc = pt.matrixTransform(svg.getScreenCTM().inverse());
    if (loc.x < ML || loc.x > W - MR) return hide();
    let best = 0, bd = Infinity;
    D.t.forEach((t, k) => { const d = Math.abs(X(t) - loc.x); if (d < bd) { bd = d; best = k; } });
    show(best, e.clientX, e.clientY);
  });
  svg.addEventListener("pointerleave", hide);
})();

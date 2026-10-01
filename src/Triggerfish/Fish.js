// The machines' fish, and the signal-flow chart's other creatures (the kraken
// for purerl-tidal, the lanternfish for link-spike, the grouper for Itajara)
// and its headphones: one SVG sprite of <symbol>s, added to the page once, so
// any page can draw a machine's fish with <use href="#sp-<slot>">. Drawn in the
// identity study (docs/kb/plans/assets/reef-specimens.html).
const SPRITE = `<svg id="tf-fish" width="0" height="0" style="position:absolute" aria-hidden="true">
  <defs>
    <path id="p-body" d="M14,63 C20,50 40,30 72,26 C100,22 124,32 142,46 L158,56 L158,70 L142,80 C124,94 100,104 72,100 C44,96 22,78 14,66 Z"/>
    <path id="p-dorsal" d="M100,27 C116,12 138,18 150,43 L141,46 C130,36 116,31 100,31 Z"/>
    <path id="p-anal" d="M100,99 C116,112 138,106 150,83 L141,80 C130,90 116,95 100,95 Z"/>
    <path id="p-spine" d="M63,29 L69,4 L76,27 Z M79,26 L82,15 L85,25 Z"/>
    <path id="p-pectoral" d="M72,62 C82,54 92,58 90,67 C84,70 77,69 72,62 Z"/>
    <path id="t-crescent" d="M156,57 C168,46 180,36 192,30 C182,50 182,76 192,96 C180,90 168,80 156,69 Z"/>
    <path id="t-lyre" d="M156,57 C170,44 184,24 198,10 C176,44 176,82 198,116 C184,102 170,82 156,69 Z"/>
    <path id="t-round" d="M156,57 L180,46 C188,56 188,70 180,80 L156,69 Z"/>
    <clipPath id="c-body"><use href="#p-body"/></clipPath>
    <clipPath id="c-moon"><path d="M24,74 L52,16 C92,10 132,32 152,56 L164,58 L164,68 L152,70 C132,94 92,110 52,106 Z"/></clipPath>

    <linearGradient id="g-odonus" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0" stop-color="#23306f"/><stop offset=".6" stop-color="#2447a0"/><stop offset="1" stop-color="#1c2f78"/>
    </linearGradient>
    <linearGradient id="g-vetula" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0" stop-color="#a88a2c"/><stop offset=".45" stop-color="#b99a3a"/><stop offset=".62" stop-color="#7f8d86"/><stop offset="1" stop-color="#5d7b98"/>
    </linearGradient>
    <linearGradient id="g-balistes" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0" stop-color="#7d846a"/><stop offset="1" stop-color="#a3a88f"/>
    </linearGradient>
    <linearGradient id="g-moon" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0" stop-color="#eef1f4"/><stop offset=".5" stop-color="#c3cad3"/><stop offset=".78" stop-color="#a99bc6"/><stop offset="1" stop-color="#8d98a8"/>
    </linearGradient>
  </defs>

  <!-- Odonus niger: blue-black, pale-edged fins, red teeth, the lyre tail. -->
  <symbol id="sp-odonus" viewBox="0 0 200 120">
    <use href="#t-lyre" fill="#1d2c6c" stroke="#8fb4f0" stroke-width="2" style="display:var(--fine,inline)"/>
    <use href="#t-lyre" fill="#1d2c6c"/>
    <use href="#p-dorsal" fill="#26378a"/>
    <use href="#p-anal" fill="#26378a"/>
    <g style="display:var(--fine,inline)" fill="none" stroke="#8fb4f0" stroke-width="1.6">
      <path d="M104,28 C118,16 136,20 148,42"/><path d="M104,98 C118,110 136,106 148,84"/>
    </g>
    <use href="#p-spine" fill="#1d2c6c"/>
    <use href="#p-body" fill="url(#g-odonus)"/>
    <g clip-path="url(#c-body)" style="display:var(--detail,inline)">
      <path d="M40,30 C60,50 64,80 46,100" fill="none" stroke="#3b5cc0" stroke-width="7" opacity=".35"/>
    </g>
    <use href="#p-pectoral" fill="#2d3f94"/>
    <path d="M65,54 C61,60 61,68 65,74" fill="none" stroke="#12194a" stroke-width="1.5" style="display:var(--detail,inline)"/>
    <path d="M13,60 L20,62 L14,64 Z M14,65 L20,66 L15,69 Z" fill="#d6352b"/>
    <circle cx="56" cy="45" r="6" fill="#0c1233"/><circle cx="56" cy="45" r="2.6" fill="#8fb4f0"/>
  </symbol>

  <!-- Balistes vetula: gold over blue-grey, two blue face lines, dark lines round the eye. -->
  <symbol id="sp-vetula" viewBox="0 0 200 120">
    <use href="#t-lyre" fill="#8a7a3c"/>
    <path d="M196,12 C200,6 202,2 204,-2 M196,114 C200,120 202,124 204,128" stroke="#2fa3d6" stroke-width="1.6" fill="none" style="display:var(--fine,inline)"/>
    <use href="#p-dorsal" fill="#9a8a3e"/>
    <use href="#p-anal" fill="#6f8aa0"/>
    <g style="display:var(--fine,inline)" fill="none" stroke="#2fa3d6" stroke-width="1.4">
      <path d="M104,28 C118,16 136,20 148,42"/><path d="M104,98 C118,110 136,106 148,84"/>
      <path d="M160,58 C172,50 184,34 196,14"/><path d="M160,68 C172,76 184,92 196,112"/>
    </g>
    <use href="#p-spine" fill="#7c6c2c"/>
    <use href="#p-body" fill="url(#g-vetula)"/>
    <g clip-path="url(#c-body)" fill="none" stroke-linecap="round">
      <path d="M14,70 C38,78 60,80 86,74" stroke="#2fa3d6" stroke-width="4"/>
      <path d="M18,78 C40,90 66,92 94,84" stroke="#2fa3d6" stroke-width="3.2" style="display:var(--detail,inline)"/>
      <g stroke="#3b3320" stroke-width="1.4" style="display:var(--detail,inline)">
        <path d="M49,36 L44,30"/><path d="M56,36 L56,29"/><path d="M63,37 L68,31"/><path d="M47,51 L41,55"/><path d="M65,51 L71,55"/>
      </g>
    </g>
    <use href="#p-pectoral" fill="#9aa8a6"/>
    <path d="M65,54 C61,60 61,68 65,74" fill="none" stroke="#4a4526" stroke-width="1.5" style="display:var(--detail,inline)"/>
    <circle cx="56" cy="45" r="6" fill="#2d2716"/><circle cx="56" cy="45" r="2.6" fill="#d8c27a"/>
  </symbol>

  <!-- Balistes capriscus: olive-grey with darker saddles and blue dots. -->
  <symbol id="sp-balistes" viewBox="0 0 200 120">
    <use href="#t-crescent" fill="#6e755e"/>
    <use href="#p-dorsal" fill="#737a62"/>
    <use href="#p-anal" fill="#858b72"/>
    <use href="#p-spine" fill="#5d6450"/>
    <use href="#p-body" fill="url(#g-balistes)"/>
    <g clip-path="url(#c-body)">
      <path d="M78,20 L96,20 L90,58 L74,58 Z" fill="#5c634d" opacity=".55"/>
      <path d="M106,20 L124,26 L114,60 L100,58 Z" fill="#5c634d" opacity=".45" style="display:var(--detail,inline)"/>
      <path d="M132,30 L146,42 L134,62 L122,60 Z" fill="#5c634d" opacity=".4" style="display:var(--detail,inline)"/>
      <g fill="#7fa6c4" style="display:var(--fine,inline)">
        <circle cx="84" cy="38" r="1.6"/><circle cx="96" cy="44" r="1.6"/><circle cx="110" cy="36" r="1.6"/><circle cx="122" cy="46" r="1.6"/><circle cx="104" cy="52" r="1.6"/><circle cx="90" cy="54" r="1.6"/><circle cx="134" cy="52" r="1.6"/>
      </g>
    </g>
    <use href="#p-pectoral" fill="#6e755e"/>
    <path d="M65,54 C61,60 61,68 65,74" fill="none" stroke="#4d5340" stroke-width="1.5" style="display:var(--detail,inline)"/>
    <circle cx="56" cy="45" r="6" fill="#2a2e22"/><circle cx="56" cy="45" r="2.6" fill="#c7cdb1"/>
  </symbol>

  <!-- Balistoides conspicillum, reduced: black, three white spots, a yellow mouth. -->
  <symbol id="sp-conspicillum" viewBox="0 0 200 120">
    <use href="#t-round" fill="#17171b"/>
    <use href="#p-dorsal" fill="#1d1d22"/>
    <use href="#p-anal" fill="#1d1d22"/>
    <use href="#p-spine" fill="#17171b"/>
    <use href="#p-body" fill="#17171b"/>
    <g clip-path="url(#c-body)" fill="#f2f2ee">
      <circle cx="74" cy="94" r="11"/><circle cx="102" cy="97" r="12"/><circle cx="130" cy="86" r="11"/>
    </g>
    <path d="M10,59 C15,56 21,59 22,64 C21,69 15,72 10,69 Z" fill="#e6c02e"/>
    <circle cx="56" cy="45" r="6" fill="#0a0a0c"/><circle cx="56" cy="45" r="2.6" fill="#e6c02e"/>
  </symbol>

  <!-- Quadrat: a boxfish (Ostracion). A near-square body whose shell is a grid of plates. -->
  <symbol id="sp-quadrat" viewBox="0 0 200 120">
    <path d="M148,58 C162,42 180,40 188,46 C183,58 183,68 188,80 C180,86 162,84 148,68 Z" fill="#a8471f"/>
    <path d="M110,20 C116,6 130,6 136,20 Z M110,100 C116,114 130,114 136,100 Z" fill="#a8471f"/>
    <rect x="38" y="18" width="112" height="84" rx="18" fill="#c4592b"/>
    <clipPath id="c-box"><rect x="38" y="18" width="112" height="84" rx="18"/></clipPath>
    <g clip-path="url(#c-box)">
      <path d="M66,18 V102 M94,18 V102 M122,18 V102 M38,46 H150 M38,74 H150" stroke="#f3d3bd" stroke-width="3" style="display:var(--detail,inline)"/>
      <g fill="#f3d3bd" style="display:var(--fine,inline)">
        <circle cx="80" cy="32" r="3"/><circle cx="108" cy="32" r="3"/><circle cx="136" cy="32" r="3"/>
        <circle cx="80" cy="60" r="3"/><circle cx="108" cy="60" r="3"/><circle cx="136" cy="60" r="3"/>
        <circle cx="52" cy="88" r="3"/><circle cx="80" cy="88" r="3"/><circle cx="108" cy="88" r="3"/><circle cx="136" cy="88" r="3"/>
      </g>
    </g>
    <path d="M84,64 C92,58 100,62 98,70 C92,72 87,70 84,64 Z" fill="#8f3a17"/>
    <ellipse cx="33" cy="70" rx="7" ry="5.5" fill="#8f3a17"/>
    <circle cx="58" cy="40" r="8" fill="#2a1409"/><circle cx="58" cy="40" r="3.4" fill="#f3d3bd"/>
  </symbol>

  <!-- Sufflamen bridled: brown, a pale bridle line from the mouth, retired. -->
  <symbol id="sp-sufflamen" viewBox="0 0 200 120">
    <use href="#t-round" fill="#6a4c33"/>
    <path d="M180,46 C188,56 188,70 180,80" fill="none" stroke="#efe2c6" stroke-width="2" style="display:var(--detail,inline)"/>
    <use href="#p-dorsal" fill="#6f5036"/>
    <use href="#p-anal" fill="#6f5036"/>
    <use href="#p-spine" fill="#5c412b"/>
    <use href="#p-body" fill="#7b5b3e"/>
    <g clip-path="url(#c-body)">
      <path d="M16,67 C34,70 50,70 68,62" fill="none" stroke="#efe2c6" stroke-width="3.2" stroke-linecap="round"/>
      <path d="M40,94 C70,100 110,96 140,82" fill="none" stroke="#5c412b" stroke-width="8" opacity=".5" style="display:var(--detail,inline)"/>
    </g>
    <use href="#p-pectoral" fill="#6a4c33"/>
    <circle cx="56" cy="45" r="6" fill="#2a1d12"/><circle cx="56" cy="45" r="2.6" fill="#e0c79a"/>
  </symbol>

  <!-- Selene vomer, the lookdown: another family. Steep forehead, silver, long fin filaments. -->
  <symbol id="sp-selene" viewBox="0 0 200 120">
    <path d="M160,58 L192,34 L178,63 L192,92 L160,68 Z" fill="#aab3bf"/>
    <g fill="none" stroke="#8d7fb4" stroke-width="1.6" stroke-linecap="round" style="display:var(--fine,inline)">
      <path d="M86,14 C96,-2 122,-6 150,2"/><path d="M88,110 C100,122 126,124 150,118"/>
    </g>
    <path d="M24,74 L52,16 C92,10 132,32 152,56 L164,58 L164,68 L152,70 C132,94 92,110 52,106 Z" fill="url(#g-moon)"/>
    <g clip-path="url(#c-moon)" style="display:var(--detail,inline)">
      <path d="M40,40 C80,60 120,60 160,50" fill="none" stroke="#ffffff" stroke-width="10" opacity=".45"/>
    </g>
    <path d="M80,72 C88,66 96,68 96,74 C90,78 84,78 80,72 Z" fill="#b8b0d4"/>
    <circle cx="50" cy="70" r="5.5" fill="#3a3550"/><circle cx="50" cy="70" r="2.3" fill="#e6e2f4"/>
  </symbol>
  <symbol id="ic-kraken" viewBox="0 0 60 60">
    <path d="M30,2 C12,6 10,28 22,34 C26,36 34,36 38,34 C50,28 48,6 30,2 Z" style="fill:var(--kraken,#8a3a50)"/>
    <circle cx="25" cy="26" r="3" fill="#f3d36b"/><circle cx="35" cy="26" r="3" fill="#f3d36b"/>
    <g style="stroke:var(--kraken,#8a3a50)" stroke-width="3.2" fill="none" stroke-linecap="round">
      <path d="M22,34 C16,42 10,44 6,54"/><path d="M26,35 C24,46 20,50 20,58"/><path d="M34,35 C36,46 40,50 40,58"/><path d="M38,34 C44,42 50,44 54,54"/>
    </g>
  </symbol>
  <symbol id="ic-lantern" viewBox="0 0 60 36">
    <path d="M6,18 C14,6 38,4 48,14 L58,6 L56,18 L58,30 L48,22 C38,32 14,30 6,18 Z" fill="#9fb0b8"/>
    <circle cx="16" cy="16" r="3.4" fill="#f6d86b"/><circle cx="28" cy="22" r="1.6" fill="#f6d86b"/><circle cx="36" cy="22" r="1.6" fill="#f6d86b"/>
  </symbol>
  <symbol id="ic-grouper" viewBox="0 0 70 40">
    <path d="M4,20 C14,4 46,2 56,14 L68,6 L66,20 L68,34 L56,26 C46,38 14,36 4,20 Z" fill="#6a5a3c"/>
    <circle cx="14" cy="17" r="2.6" fill="#1c160c"/>
  </symbol>
  <symbol id="ic-ears" viewBox="0 0 48 48">
    <path d="M8,30 C8,14 16,6 24,6 C32,6 40,14 40,30" fill="none" style="stroke:var(--ink,#0f2530)" stroke-width="3.4"/>
    <rect x="4" y="27" width="10" height="16" rx="3" style="fill:var(--ink,#0f2530)"/><rect x="34" y="27" width="10" height="16" rx="3" style="fill:var(--ink,#0f2530)"/>
  </symbol>
</svg>`;

export const install = () => {
  if (document.getElementById("tf-fish")) return;
  const holder = document.createElement("div");
  holder.innerHTML = SPRITE;
  document.body.prepend(holder.firstElementChild);
};

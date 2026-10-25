(()=>{
'use strict';
const $=s=>document.querySelector(s);
const stage=$('#stage'), svg=$('#rail'), panel=$('#panel'), tray=$('#tray'), out=$('#out'), copyBtn=$('#copy'),
      undoBtn=$('#undo'), tidyBtn=$('#tidy'), clearBtn=$('#clear'), tin=$('#t'), res=$('#res');
const COARSE=!!(window.matchMedia&&matchMedia('(pointer:coarse)').matches);
const R=8, SLOT=COARSE?30:22, RP=16, PA=20, GV=10, KEY='regex-senro-3';
const GEO={slots:[],nodes:[],lanes:[]};

/* ---------- icons: 図柄は index.html の #icons スプライトにあり、ここでは参照だけ作る ---------- */
const ICON=Object.fromEntries([...document.querySelectorAll('#icons symbol')].map(s=>
  [s.id.slice(2),`<svg class="i" aria-hidden="true"><use href="#${s.id}"/></svg>`]));

/* ---------- model ----------
   node = {t, min, max, skip:[], back:[], ...}
   skip = 省略の脇道(min=0のとき現れる)に置かれた駅の並び
   back = 繰り返しの戻り線(max>1 か 無限のとき現れる)に置かれた駅の並び */
const esc=s=>String(s).replace(/[&<>"]/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;'}[c]));
const rxEsc=s=>s.replace(/[.*+?^${}()|[\]\\\/]/g,'\\$&');
const mk=(t,o)=>Object.assign({t,min:1,max:1,skip:[],back:[]},o);
/* 初期状態: 英語の規則的な複数形を拾う。
   前後に赤信号(後読み・先読み)を立てて単語の切れ目にし、語尾の規則を4本の分岐で描く:
   -es(s, x, z, o, ch, sh の後) / 子音+y → -ies / f, fe → -ves / それ以外は -s */
const example=()=>{
  const edge=behind=>mk('look',{behind,neg:true,lanes:[[mk('cls',{v:'\\w'})]]});
  const lit=v=>mk('lit',{v}), not=v=>mk('set',{v,neg:true});
  return {a:false,z:false,root:[
    edge(true),
    mk('set',{v:'a-z',neg:false,max:null}),
    mk('alt',{lanes:[
      [mk('alt',{lanes:[[mk('set',{v:'sxzo',neg:false})],[lit('ch')],[lit('sh')]]}),lit('es')],
      [not('aeiou'),lit('ies')],
      [lit('ves')],
      [not('s'),lit('s')]
    ]}),
    edge(false)
  ]};
};
function clean(seq){
  if(!Array.isArray(seq))return [];
  return seq.filter(n=>n&&['lit','cls','set','alt','look'].includes(n.t)).map(n=>{
    const o={t:n.t,min:Number.isFinite(n.min)?n.min:1,max:n.max===null?null:(Number.isFinite(n.max)?n.max:1),
             skip:clean(n.skip),back:clean(n.back)};
    if(n.t==='alt'||n.t==='look'){
      let L=Array.isArray(n.lanes)&&n.lanes.length?n.lanes:[[]];
      if(n.t==='look'){L=[L[0]];o.min=1;o.max=1;o.skip=[];o.back=[];o.behind=!!n.behind;o.neg=n.neg!==false}
      o.lanes=L.map(clean);
    }
    else{o.v=String(n.v==null?'':n.v); if(n.t==='set')o.neg=!!n.neg}
    if(n.lazy&&n.t!=='look')o.lazy=true;
    return o;
  });
}
let S=example(), sel=null, hist=[], editing=false;
const ui={cust:false};
const last=p=>p[p.length-1];
const pstr=p=>p.join('.');
const parse=s=>s===''?[]:s.split('.').map(Number);
/* 経路は [駅の番号, レーン番号, ...]。レーン番号 0.. は分岐のレーン、-1 は省略の脇道、-2 は戻り線 */
function seqAt(pre){
  let s=S.root;
  for(let k=0;k<pre.length;k+=2){
    const n=s[pre[k]], j=pre[k+1];
    s=j===-1?(n.skip||(n.skip=[])):j===-2?(n.back||(n.back=[])):n.lanes[j];
  }
  return s;
}
const nodeAt=p=>seqAt(p.slice(0,-1))[last(p)];
const isSel=(k,p)=>!!sel&&sel.k===k&&pstr(sel.p)===pstr(p);
const laneEntries=n=>[...(n.t==='alt'||n.t==='look'?n.lanes.map((l,j)=>[j,l]):[]),[-1,n.skip||[]],[-2,n.back||[]]];

function rng(n){const a=Math.max(0,n.min|0);const b=n.max===null?null:Math.max(n.max|0,a,1);return [a,b]}
const qs=(a,b)=>{
  if(a===1&&b===1)return '';
  if(a===0&&b===1)return '?';
  if(a===0&&b===null)return '*';
  if(a===1&&b===null)return '+';
  if(b===null)return `{${a},}`;
  if(a===b)return `{${a}}`;
  return `{${a},${b}}`;
};
function quant(n){const [a,b]=rng(n);return qs(a,b)}
const repKey=n=>({'':'1','?':'?','*':'*','+':'+'}[quant(n)]||'n');
const loopLabel=(a,b)=>b===null?(a>1?a+'+':''):(a===b?a+'×':a+'–'+b);
/* 省略でなくなった脇道・繰り返しでなくなった戻り線の駅は外す */
function fixLanes(seq){
  seq.forEach(n=>{
    const [a,b]=rng(n);
    n.skip=n.skip||[];n.back=n.back||[];
    if(a>0&&n.skip.length)n.skip=[];
    if(!(b===null||b>1)&&n.back.length)n.back=[];
    laneEntries(n).forEach(([,l])=>fixLanes(l));
  });
}

/* ---------- regex (tokens keep the node colour for the output line) ---------- */
function altTok(n,group){
  const o=[];
  if(group)o.push(['(?:','alt']);
  n.lanes.forEach((l,j)=>{if(j)o.push(['|','alt']);o.push(...seqTok(l,true))});
  if(group)o.push([')','alt']);
  return o;
}
function unitTok(n){
  if(n.t==='lit')return {t:[[rxEsc(n.v),'lit']],one:n.v.length===1};
  if(n.t==='cls')return {t:[[n.v,'cls']],one:true};
  if(n.t==='set')return {t:[['['+(n.neg?'^':'')+n.v+']','set']],one:true};
  return {t:altTok(n,true),one:true};
}
/* 通行止め: (?!…) 前を見て禁止 / (?=…) 前を見て必須 / (?<!…) (?<=…) は後ろ */
function lookTok(n){
  const body=seqTok(n.lanes[0]||[],true);
  if(!body.length)return [];
  return [['(?'+(n.behind?'<':'')+(n.neg?'!':'='),'look'],...body,[')','look']];
}
function nodeTok(n){
  if(n.t==='look')return lookTok(n);
  if(n.t==='lit'&&!n.v)return [];
  const u=unitTok(n), kind=n.t==='alt'?'alt':n.t;
  const grp=u.one?u.t:[['(?:','lit'],...u.t,[')','lit']];
  const [a,b]=rng(n), rep=b===null||b>1, opt=a===0;
  const back=rep&&n.back&&n.back.length?n.back:null, skip=opt&&n.skip&&n.skip.length?n.skip:null;
  /* 非貪欲(ポイントの定位が「抜ける」側): 回数に幅があるときだけ ? を足す。脇道があるときは脇道を先に試す */
  const lz=!!n.lazy&&(b===null||a!==b);
  const lq=(x,y)=>{const q=qs(x,y);return q&&lz&&(y===null||x!==y)?q+'?':q};
  if(!back&&!skip){const q=lq(a,b);return q?[...grp,[q,kind]]:u.t}
  let body;
  if(back){
    /* 駅 (戻り線の駅 駅){回数}  例: a(?:-a)* */
    const q=lq(Math.max(a,1)-1,b===null?null:b-1);
    body=[...u.t,['(?:','alt'],...seqTok(n.back,false),...u.t,[')','alt']];
    if(q)body.push([q,'alt']);
  }else{
    const q=lq(Math.max(a,1),b);
    body=q?[...grp,[q,kind]]:u.t;
  }
  if(skip)return lz?[['(?:','alt'],...seqTok(n.skip,true),['|','alt'],...body,[')','alt']]
                   :[['(?:','alt'],...body,['|','alt'],...seqTok(n.skip,true),[')','alt']];
  if(opt)return [['(?:','alt'],...body,[')','alt'],[lz?'??':'?','alt']];
  return body;
}
function seqTok(seq,bare){
  if(bare&&seq.length===1&&seq[0].t==='alt'&&!quant(seq[0]))return altTok(seq[0],false);
  return seq.flatMap(nodeTok);
}
function tokens(){
  const t=[];
  if(S.a)t.push(['^','term']);
  t.push(...seqTok(S.root,!(S.a||S.z)));
  if(S.z)t.push(['$','term']);
  return t;
}
const pattern=()=>tokens().map(t=>t[0]).join('');

/* ---------- layout: every box returns {w,up,down,draw(x,baselineY)} ---------- */
const track=d=>`<path class="tie" d="${d}"/><path class="rail" d="${d}"/>`;
const tw=s=>{let w=0;for(const ch of s)w+=ch.charCodeAt(0)>0x2e7f?13:7.9;return w};
const KIND_VAR={lit:'--c-lit',cls:'--c-cls',set:'--c-set'};
function labelOf(n){
  if(n.t==='lit')return n.v?n.v.replace(/ /g,'·'):'…';
  if(n.t==='set')return '['+(n.neg?'^':'')+n.v+']';
  return n.v||'';
}
function layoutBox(n,path){
  const lab=labelOf(n), w=Math.max(36,tw(lab)+24), h=34;
  const kind=n.t==='cls'&&n.v==='.'?'cls':n.t;
  return {w,up:h/2,down:h/2,draw(x,y){
    const sel=isSel('n',path);
    GEO.nodes.push({p:path,x,y,w,h,t:n.t});
    /* 「〜以外」の駅: 通行止めの札を角に付け、箱を薄く赤くする */
    const ng=n.t==='set'&&n.neg, bx=x+w-3, by=y-h/2+3;
    return `<g class="node k-${kind}${ng?' neg':''}${sel?' sel':''}" data-a="n" data-p="${pstr(path)}" tabindex="0" role="button" aria-label="${esc(lab)}${ng?' (negated)':''}" style="--c:var(${KIND_VAR[kind]})">`+
      (sel?`<rect class="halo" x="${x-4}" y="${y-h/2-4}" width="${w+8}" height="${h+8}" rx="11"/>`:'')+
      `<rect class="box" x="${x}" y="${y-h/2}" width="${w}" height="${h}" rx="7"/>`+
      `<text x="${x+w/2}" y="${y+.5}" text-anchor="middle">${esc(lab)}</text>`+
      (ng?`<g class="nb"><circle cx="${bx}" cy="${by}" r="8"/><rect x="${bx-4.5}" y="${by-1.5}" width="9" height="3" rx="1.5"/></g>`:'')+`</g>`;
  }};
}
function layoutSlot(p,empty,rev){
  const w=empty?56:SLOT;
  return {w,up:17,down:17,draw(x,y){
    const sel=isSel('s',p), cx=x+w/2;
    GEO.slots.push({p,cx,cy:y});
    /* すき間ごとに進行方向の矢尻(戻り線では左向き) */
    const d=rev?-1:1;
    return `<g class="slot${empty?' empty':''}${sel?' sel':''}" data-a="s" data-p="${pstr(p)}" tabindex="0" role="button" aria-label="Drop here">`+
      `<rect class="hit" x="${x}" y="${y-20}" width="${w}" height="40"/>`+
      (empty?`<rect class="ph" x="${x+3}" y="${y-15}" width="${w-6}" height="30" rx="7"/>`
            :`<polygon class="sarw" points="${cx-4*d},${y-5} ${cx+4*d},${y} ${cx-4*d},${y+5}"/>`)+
      `<circle cx="${cx}" cy="${y}" r="${sel?8:6}"/><path d="M${cx-3} ${y}h6M${cx} ${y-3}v6"/></g>`;
  }};
}
/* rev=true は戻り線用。進行方向(右→左)に合わせて駅を逆順に描く */
function layoutSeq(seq,path,rev,aleft){
  let items=[];
  if(!seq.length)items.push(layoutSlot(path.concat(0),true));
  else for(let i=0;i<=seq.length;i++){
    items.push(layoutSlot(path.concat(i),false,rev||aleft));
    if(i<seq.length)items.push(layoutNode(seq[i],path.concat(i)));
  }
  if(rev)items=items.reverse();
  let w=0,up=14,down=14;
  items.forEach(it=>{w+=it.w;up=Math.max(up,it.up);down=Math.max(down,it.down)});
  return {w,up,down,draw(x,y){let s='',cx=x;items.forEach(it=>{s+=it.draw(cx,y);cx+=it.w});return s}};
}
function layoutAlt(n,path){
  const lanes=n.lanes.map((l,j)=>layoutSeq(l,path.concat(j)));
  const maxW=Math.max(...lanes.map(l=>l.w)), W=maxW+2*PA;
  const ys=[0];
  for(let j=1;j<lanes.length;j++)ys.push(ys[j-1]+lanes[j-1].down+GV+lanes[j].up);
  const lastJ=lanes.length-1, up0=lanes[0].up;
  const addY=ys[lastJ]+lanes[lastJ].down+GV+8;
  return {w:W,up:up0+12,down:addY+14,draw(x,y){
    const sel=isSel('n',path), id=pstr(path);
    const top=y-up0-10, bot=y+addY+10, cx=x+W/2;
    let s=`<g class="alt${sel?' sel':''}" data-a="n" data-p="${id}" tabindex="0" role="button" aria-label="Branch">`+
      `<rect class="frame" x="${x+1}" y="${top}" width="${W-2}" height="${bot-top}" rx="14"/></g>`;
    if(lanes.length>1){
      const yl=y+ys[lastJ];
      s+=track(`M${x} ${y}a${R} ${R} 0 0 1 ${R} ${R}V${yl-R}a${R} ${R} 0 0 0 ${R} ${R}H${x+W-2*R}a${R} ${R} 0 0 0 ${R} ${-R}V${y+R}a${R} ${R} 0 0 1 ${R} ${-R}`);
      for(let j=1;j<lastJ;j++){
        const yj=y+ys[j];
        s+=track(`M${x+R} ${yj-R}a${R} ${R} 0 0 0 ${R} ${R}H${x+W-2*R}a${R} ${R} 0 0 0 ${R} ${-R}`);
      }
      s+=`<path class="stub" d="M${x+R} ${yl-R}V${y+addY-9}"/>`;
    }
    lanes.forEach((l,j)=>{s+=l.draw(x+PA+(maxW-l.w)/2,y+ys[j])});
    const ax=x+R, ay=y+addY;
    GEO.lanes.push({p:path,cx:ax,cy:ay});
    s+=`<g class="add" data-a="addlane" data-p="${id}" tabindex="0" role="button" aria-label="Add lane">`+
      `<circle class="hit" cx="${ax}" cy="${ay}" r="14"/><circle class="dot" cx="${ax}" cy="${ay}" r="8"/>`+
      `<path d="M${ax-3} ${ay}h6M${ax} ${ay-3}v6"/></g>`;
    return s;
  }};
}
/* 信号(先読み・後読み): 列車は信号の前で止まったまま進まない。信号から吹き出しを出し、確かめる並びを描く。
   吹き出しの中の小さな信号が現在位置。先読みは「🚥→この先」、後読みは「ここまで→🚥」。どちらも左→右に読む。
   赤(否定)= この並びなら止まる / 青(肯定)= この並びのときだけ進む */
function layoutLook(n,path){
  const behind=!!n.behind, neg=n.neg!==false;
  const sp=layoutSeq(n.lanes[0],path.concat(0));
  const G=15, W=sp.w+45, hu=sp.up+4, hd=sp.down+4, TAIL=9, yS=-(G+TAIL+hd);
  return {w:W,up:-yS+hu,down:17,draw(x,y){
    const id=pstr(path), sel=isSel('n',path), yl=y+yS;
    const bx=behind?x+W-18:x+18, L=x+2, Rb=x+W-2, sx=behind?bx-15-sp.w:bx+15;
    const r=8, top=yl-hu, bot=yl+hd, tip=y-G-1;
    const bub=`M${L+r} ${top}H${Rb-r}a${r} ${r} 0 0 1 ${r} ${r}V${bot-r}a${r} ${r} 0 0 1 ${-r} ${r}H${bx+6}L${bx} ${tip}L${bx-6} ${bot}H${L+r}a${r} ${r} 0 0 1 ${-r} ${-r}V${top+r}a${r} ${r} 0 0 1 ${r} ${-r}Z`;
    const rail=behind?`M${L+6} ${yl}H${bx-6}`:`M${bx+6} ${yl}H${Rb-6}`;
    /* 信号機: 上=赤、下=青。吹き出しの並びに当てはまったときに点く方だけ点灯 */
    const head=(cx,cy,w,h,lr)=>`<circle class="lr${neg?' on':''}" cx="${cx}" cy="${cy-h/4}" r="${lr}"/><circle class="lgn${neg?'':' on'}" cx="${cx}" cy="${cy+h/4}" r="${lr}"/>`;
    const lamp=head(bx,y,18,30,4.6);
    const lab=(neg?'Red signal: stop if ':'Green signal: go only if ')+(behind?'the text so far ends with this':'the text ahead starts with this');
    return `<g class="node look${sel?' sel':''}" data-a="n" data-p="${id}" tabindex="0" role="button" aria-label="${lab}" style="--c:var(${neg?'--c-look':'--good'})">`+
      `<path class="bubble" d="${bub}"/><path class="brail" d="${rail}"/>`+
      `<rect class="sig" x="${bx-5.5}" y="${yl-10}" width="11" height="20" rx="5.5"/>`+'<g class="mini">'+head(bx,yl,11,20,2.8)+'</g>'+
      (sel?`<rect class="halo" x="${bx-13}" y="${y-19}" width="26" height="38" rx="13"/>`:'')+
      `<rect class="box" x="${bx-9}" y="${y-G}" width="18" height="${2*G}" rx="9"/>`+lamp+`<title>${lab}</title></g>`+
      sp.draw(sx,yl);
  }};
}
/* 省略(?)の脇道は上、繰り返しの戻り線は下。どちらにも駅を置ける */
function layoutNode(n,path){
  const inner=n.t==='look'?layoutLook(n,path):n.t==='alt'?layoutAlt(n,path):layoutBox(n,path);
  const [a,b]=rng(n), skip=a===0, loop=b===null||b>1;
  if(!skip&&!loop)return inner;
  const sl=skip?layoutSeq(n.skip||[],path.concat(-1)):null;
  const bl=loop?layoutSeq(n.back||[],path.concat(-2),true):null;
  const lab=loop?loopLabel(a,b):'';
  const W=Math.max(inner.w,sl?sl.w:0,bl?bl.w:0)+2*RP;
  const yS=sl?-(inner.up+8+sl.down):0, yL=bl?inner.down+8+bl.up:0;
  const up=sl?(-yS+sl.up):inner.up, down=bl?(yL+bl.down+(lab?16:0)):inner.down;
  return {w:W,up,down,draw(x,y){
    let s='';
    /* 非貪欲: 後回しにする道を「細い側線」で描く。太い本線が先に試す道。
       繰り返し → 戻り線が細い(なるべく回らない) / 省略 → 駅を通る線が細い(なるべく通らない) */
    const lz=!!n.lazy&&(b===null||a!==b);
    if(lz&&skip)s+=`<path class="offmask" d="M${x+1} ${y}H${x+W-1}"/><path class="thin" d="M${x} ${y}H${x+W}"/>`;
    if(sl){
      const yl=y+yS;
      s+=track(`M${x} ${y}a${R} ${R} 0 0 0 ${R} ${-R}V${yl+R}a${R} ${R} 0 0 1 ${R} ${-R}H${x+W-2*R}a${R} ${R} 0 0 1 ${R} ${R}V${y-R}a${R} ${R} 0 0 0 ${R} ${R}`);
      s+=sl.draw(x+(W-sl.w)/2,yl);
    }
    if(bl){
      const yl=y+yL, ax=x+W-R, ay=y+yL/2;
      const dB=`M${x+W-2*R} ${y}a${R} ${R} 0 0 1 ${R} ${R}V${yl-R}a${R} ${R} 0 0 1 ${-R} ${R}H${x+2*R}a${R} ${R} 0 0 1 ${-R} ${-R}V${y+R}a${R} ${R} 0 0 1 ${R} ${-R}`;
      s+=lz?`<path class="thin" d="${dB}"/>`:track(dB);
      s+=`<path class="arw" d="M${ax-4.5} ${ay-3}L${ax+4.5} ${ay-3}L${ax} ${ay+4}z"/>`;
      s+=bl.draw(x+(W-bl.w)/2,yl);
      if(lab)s+=`<text class="lbl" x="${x+W/2}" y="${yl+bl.down+10}" text-anchor="middle">${esc(lab)}</text>`;
    }
    return s+inner.draw(x+(W-inner.w)/2,y);
  }};
}

/* ---------- canvas ---------- */
function fixSel(){
  if(!sel)return;
  try{
    if(sel.k==='n'){if(!nodeAt(sel.p))sel=null}
    else{const q=seqAt(sel.p.slice(0,-1));if(!q||last(sel.p)>q.length)sel=null}
  }catch(e){sel=null}
}
function drawCanvas(){
  const ae=document.activeElement;
  let fk=null;
  if(ae&&svg.contains(ae)&&ae.dataset&&ae.dataset.a)fk=[ae.dataset.a,ae.dataset.p||''];
  fixSel();
  const L=layoutSeq(S.root,[]);
  const m=6, T=56, up=Math.max(L.up,17), down=Math.max(L.down,17);
  const W=m*2+T*2+L.w, y=12+up, H=12+up+down+12;
  let s=track(`M${m+T-2} ${y}H${W-m-T+2}`);
  GEO.slots.length=GEO.nodes.length=GEO.lanes.length=0;
  s+=L.draw(m+T,y);
  /* 線路の両端。^ / $ が付いていれば車止め(始発駅・終着駅)、付いていなければ線路が画面の外へ続いていく(どこからでも乗り入れ・どこへでも直通) */
  const term=(id,left,on)=>{
    const ch=left?'^':'$', e=left?m:W-m, d=left?1:-1, inner=left?m+T:W-m-T, px=e+d*14, bx=e+d*36;
    const lab=left?(on?'Terminus: start of string':'Open start: can begin anywhere'):(on?'Terminus: end of string':'Open end: can stop anywhere');
    const stop=`<g class="bstop"><path class="bleg" d="M${bx+d*4} ${y-9}L${bx+d*14} ${y}M${bx+d*4} ${y+9}L${bx+d*14} ${y}"/>`+
      `<rect x="${bx-4}" y="${y-14}" width="8" height="28" rx="2"/><path class="bstripe" d="M${bx-4} ${y-7}h8M${bx-4} ${y}h8M${bx-4} ${y+7}h8"/></g>`;
    let g=`<g class="term${on?' on':' off'}" data-a="${id}" tabindex="0" role="button" aria-pressed="${on}" aria-label="${ch} ${lab}"><title>${ch} ${lab}</title>`+
      `<rect class="hit" x="${Math.min(e,inner)}" y="${y-22}" width="${T}" height="44"/>`;
    if(on)g+=track(`M${bx} ${y}H${inner}`)+stop;
    else g+=`<g mask="url(#fade-${id})">${track(`M${e+d*26} ${y}H${inner}`)}</g><g class="ghost">${stop}</g>`;
    /* 駅名標のような札に ^ / $ を大きく書く */
    return g+`<rect class="plate" x="${px-14}" y="${y-14}" width="28" height="28" rx="7"/><text class="tlab" x="${px}" y="${y+1}" text-anchor="middle">${ch}</text></g>`;
  };
  const fade=(id,x1,x2)=>`<linearGradient id="fg-${id}" gradientUnits="userSpaceOnUse" x1="${x1}" y1="0" x2="${x2}" y2="0"><stop offset="0" stop-color="#fff" stop-opacity="0"/><stop offset="1" stop-color="#fff"/></linearGradient>`+
    `<mask id="fade-${id}" maskUnits="userSpaceOnUse" x="0" y="0" width="${W}" height="${H}"><rect x="0" y="0" width="${W}" height="${H}" fill="url(#fg-${id})"/></mask>`;
  s=`<defs>${fade('start',m+26,m+T-6)}${fade('goal',W-m-26,W-m-T+6)}</defs>`+s;
  s+=term('start',true,S.a)+term('goal',false,S.z);
  svg.setAttribute('width',W);svg.setAttribute('height',H);svg.setAttribute('viewBox',`0 0 ${W} ${H}`);
  svg.innerHTML=s;
  if(fk){
    const el=[...svg.querySelectorAll('[data-a]')].find(g=>g.dataset.a===fk[0]&&(g.dataset.p||'')===fk[1]);
    if(el)el.focus({preventScroll:true});
  }
  reveal();
}
function reveal(){
  const el=svg.querySelector('.node.sel,.slot.sel');
  if(!el)return;
  const a=el.getBoundingClientRect(), b=stage.getBoundingClientRect(), m=24;
  if(a.width===0)return;
  if(a.left<b.left+m)stage.scrollLeft-=b.left+m-a.left;
  else if(a.right>b.right-m)stage.scrollLeft+=a.right-(b.right-m);
  if(a.top<b.top+m)stage.scrollTop-=b.top+m-a.top;
  else if(a.bottom>b.bottom-m)stage.scrollTop+=a.bottom-(b.bottom-m);
}

/* ---------- test (matching runs in a worker so a runaway pattern cannot freeze the page) ---------- */
const WSRC="onmessage=function(e){var p=e.data.p,t=e.data.t;try{var re=new RegExp(p,'g'),r=[],m,n=0;while(n<300&&(m=re.exec(t))){if(m[0]===''){re.lastIndex++;continue}r.push([m.index,m[0].length]);n++}postMessage({r:r})}catch(x){postMessage({r:[]})}}";
let WURL=null, wk=null, job=0, tmo=0;
function syncRanges(p,t){
  const r=[];try{const re=new RegExp(p,'g');let m,n=0;while(n<300&&(m=re.exec(t))){if(m[0]===''){re.lastIndex++;continue}r.push([m.index,m[0].length]);n++}}catch(e){}
  return r;
}
function runTest(pat,err){
  const text=tin.value;
  clearTimeout(tmo);
  if(wk){wk.terminate();wk=null}
  const id=++job;
  if(!text||!pat){res.hidden=true;tin.className='';return}
  res.hidden=false;
  if(err){res.textContent=err.replace(/^Invalid regular expression: /,'');tin.className='ng';return}
  const done=(r,timeout)=>{
    if(id!==job)return;
    if(timeout){res.textContent='Timed out';tin.className='ng';return}
    let h='',lastI=0;
    r.forEach(([i,l])=>{h+=esc(text.slice(lastI,i))+'<mark>'+esc(text.slice(i,i+l))+'</mark>';lastI=i+l});
    res.innerHTML=h+esc(text.slice(lastI));
    tin.className=r.length?'ok':'ng';
  };
  try{
    if(!WURL)WURL=URL.createObjectURL(new Blob([WSRC],{type:'text/javascript'}));
    const w=new Worker(WURL);wk=w;
    w.onmessage=e=>{clearTimeout(tmo);done(e.data.r)};
    w.onerror=()=>{clearTimeout(tmo);done(syncRanges(pat,text))};
    w.postMessage({p:pat,t:text});
    tmo=setTimeout(()=>{if(wk){wk.terminate();wk=null}done(null,true)},400);
  }catch(x){done(syncRanges(pat,text))}
}
function drawOut(){
  const tk=tokens(), pat=tk.map(t=>t[0]).join('');
  let err='';
  try{new RegExp(pat)}catch(e){err=e.message}
  out.innerHTML=pat?tk.map(([s,k])=>`<span class="k-${k}">${esc(s)}</span>`).join(''):'<span class="mute">—</span>';
  out.classList.toggle('bad',!!err);
  out.title=err;
  copyBtn.disabled=!pat;
  runTest(pat,err);
}
/* ---------- 線路の整理: 意味(マッチ結果)を変えずに図を簡単にする ----------
   空の駅を外す / 隣り合う文字駅をつなぐ / 同じ駅の連続を回数にまとめる / 一本道のグループをほどく /
   重なった繰り返しをまとめる / 同じ道を一本にする / 空の道は「省略」に / 1文字だけの分岐は [ ] に / 分岐の共通の頭と尻尾をくくり出す */
const noLanes=n=>!(n.skip&&n.skip.length)&&!(n.back&&n.back.length);
const plain=n=>{const [a,b]=rng(n);return a===1&&b===1&&noLanes(n)};
const unitKey=n=>n.t==='cls'?'c'+n.v:n.t==='set'?'s'+(n.neg?'^':'')+n.v:n.t==='lit'&&n.v.length===1?'l'+n.v:null;
const setEsc=c=>/[\]\\^-]/.test(c)?'\\'+c:c;
function setPart(n){
  if(!plain(n))return null;
  if(n.t==='lit'&&n.v.length===1)return setEsc(n.v);
  if(n.t==='cls'&&/^\\[dwsDWS]$/.test(n.v))return n.v;
  if(n.t==='set'&&!n.neg&&n.v)return n.v;
  return null;
}
function addQ(p,n){
  const [a1,b1]=rng(p),[a2,b2]=rng(n);
  p.min=a1+a2;p.max=b1===null||b2===null?null:b1+b2;
}
function optSeq(seq){
  const out=[];
  const push=n=>{
    const pv=out[out.length-1];
    if(pv&&pv.t==='lit'&&n.t==='lit'&&plain(pv)&&plain(n)){pv.v+=n.v;return}
    if(pv&&noLanes(pv)&&noLanes(n)&&!pv.lazy&&!n.lazy&&unitKey(pv)&&unitKey(pv)===unitKey(n)){addQ(pv,n);return}
    out.push(n);
  };
  for(const n of seq){
    n.skip=optSeq(n.skip||[]);n.back=optSeq(n.back||[]);
    if(n.lanes)n.lanes=n.lanes.map(optSeq);
    if(n.t==='lit'&&!n.v)continue;
    if(n.t==='look'){if(lookTok(n).length)push(n);continue}
    if(n.t!=='alt'){push(n);continue}
    const seen=new Set();
    n.lanes=n.lanes.filter(l=>{const k=JSON.stringify(l);if(seen.has(k))return false;seen.add(k);return true});
    const [qa,qb]=rng(n), bare=qa===1&&qb===1&&noLanes(n);
    const pre=[],post=[];
    /* 共通の頭・尻尾(分配則)は優先度を下げる: 駅を丸ごと共有しているときだけくくり出し、文字の途中では切らない。
       くくり出した後に空になる道は、最後の1本だけ許す(=省略になる)。繰り返し付きの分岐ではグループの中でくくる */
    const same=(x,y)=>JSON.stringify(x)===JSON.stringify(y);
    const okEmpty=ls=>ls.every((l,j)=>l.length||j===ls.length-1);
    if(n.lanes.length>1){
      let k=0;const m=Math.min(...n.lanes.map(l=>l.length));
      while(k<m&&n.lanes.every(l=>same(l[k],n.lanes[0][k])))k++;
      while(k>0&&!okEmpty(n.lanes.map(l=>l.slice(k))))k--;
      if(k){
        const head=n.lanes[0].slice(0,k);n.lanes=n.lanes.map(l=>l.slice(k));
        if(bare)pre.push(...head);else n.lanes=[[...head,mk('alt',{lanes:n.lanes})]];
      }
    }
    if(n.lanes.length>1&&bare){
      let k=0;const m=Math.min(...n.lanes.map(l=>l.length));
      while(k<m&&n.lanes.every(l=>same(l[l.length-1-k],n.lanes[0][n.lanes[0].length-1-k])))k++;
      while(k>0&&!okEmpty(n.lanes.map(l=>l.slice(0,l.length-k))))k--;
      if(k){const tail=n.lanes[0].slice(-k);n.lanes=n.lanes.map(l=>l.slice(0,l.length-k));post.push(...tail)}
    }
    pre.forEach(push);
    let r=n;
    if(n.lanes.length>1&&!last(n.lanes).length&&n.lanes.slice(0,-1).every(l=>l.length)&&bare){n.lanes.pop();n.min=0;n.max=1}
    if(n.lanes.length>1&&n.lanes.every(l=>l.length===1&&setPart(l[0])!==null)){
      r=mk('set',{v:n.lanes.map(l=>setPart(l[0])).join(''),neg:false,min:n.min,max:n.max,skip:n.skip,back:n.back});
      if(n.lazy)r.lazy=true;
    }else if(n.lanes.length===1){
      const L=n.lanes[0], [a2,b2]=rng(n);
      if(a2===1&&b2===1&&noLanes(n)){L.forEach(push);post.forEach(push);continue}
      if(L.length===1&&noLanes(n)&&noLanes(L[0])&&L[0].t!=='look'&&!n.lazy&&!L[0].lazy){
        const x=L[0],[a1,b1]=rng(x);
        if(a1<=1){x.min=a1*a2;x.max=b1===null||b2===null?null:b1*b2;if(x.max===0){continue}r=x}
      }
    }
    push(r);post.forEach(push);
  }
  return out;
}
function tidy(root){
  let s=JSON.stringify(root);
  for(let i=0;i<12;i++){root=optSeq(root);fixLanes(root);const t=JSON.stringify(root);if(t===s)break;s=t}
  return root;
}
const update=()=>{fixLanes(S.root);drawCanvas();drawOut();
  try{tidyBtn.disabled=JSON.stringify(tidy(JSON.parse(JSON.stringify(S.root))))===JSON.stringify(S.root)}catch(e){tidyBtn.disabled=true}};
function save(){try{localStorage.setItem(KEY,JSON.stringify({S,t:tin.value}))}catch(e){}}
function load(){
  try{
    const j=JSON.parse(localStorage.getItem(KEY));
    if(j&&j.S&&Array.isArray(j.S.root)){S={a:!!j.S.a,z:!!j.S.z,root:clean(j.S.root)};tin.value=String(j.t||'').slice(0,120);return true}
  }catch(e){}
  return false;
}

/* ---------- editing ---------- */
const push=()=>{hist.push(JSON.stringify(S));if(hist.length>60)hist.shift();undoBtn.disabled=false};
function commit(fn){push();fn();save();update()}
function select(s,focusVal){sel=s;ui.cust=false;drawCanvas();syncPanel(focusVal)}

function mkNew(kind){
  if(kind==='lit')return mk('lit',{v:'a'});
  if(kind==='set')return mk('set',{v:'a-z',neg:false});
  if(kind==='alt')return mk('alt',{lanes:[[],[]]});
  if(kind==='look')return mk('look',{lanes:[[]],behind:false,neg:true});
  return mk('cls',{v:kind});
}
function findPath(node,seq,pre){
  seq=seq||S.root;pre=pre||[];
  for(let i=0;i<seq.length;i++){
    const n=seq[i];
    if(n===node)return pre.concat(i);
    for(const [j,l] of laneEntries(n)){const r=findPath(node,l,pre.concat(i,j));if(r)return r}
  }
  return null;
}
function inside(node,ref){
  if(node===ref)return true;
  return laneEntries(node).some(([,l])=>l===ref||l.some(c=>inside(c,ref)));
}
function afterAdd(n){
  const p=findPath(n);
  if(!p)return select(null);
  if(n.t==='alt')select({k:'s',p:p.concat(0,0)});
  else if(n.t==='look')select({k:'n',p});
  else select({k:'n',p},n.t==='lit'||n.t==='set');
}
function placeNew(kind,seq,i){const n=mkNew(kind);commit(()=>seq.splice(i,0,n));afterAdd(n)}
/* 駅を別の駅の上へ落とすと、並行する道(分岐)になる */
function branchWith(kind,p){
  const n=mkNew(kind);
  commit(()=>{const seq=seqAt(p.slice(0,-1)),i=last(p);seq[i]=mk('alt',{lanes:[[seq[i]],[n]]})});
  afterAdd(n);
}
function tapAdd(kind){
  let seq=S.root,i=S.root.length;
  if(sel){seq=seqAt(sel.p.slice(0,-1));i=last(sel.p)+(sel.k==='n'?1:0)}
  placeNew(kind,seq,i);
}
function wrapNode(p){
  commit(()=>{const seq=seqAt(p.slice(0,-1)),i=last(p);seq[i]=mk('alt',{lanes:[[seq[i]],[]]})});
  select({k:'s',p:p.concat(1,0)});
}
function addLane(p){
  commit(()=>{nodeAt(p).lanes.push([])});
  select({k:'s',p:p.concat(nodeAt(p).lanes.length-1,0)});
}
function rmLane(){
  const p=sel.p, altP=p.slice(0,-2), j=p[p.length-2];
  commit(()=>{
    const alt=nodeAt(altP);
    alt.lanes.splice(j,1);
    if(alt.lanes.length===1&&quant(alt)===''&&!alt.skip.length&&!alt.back.length){seqAt(altP.slice(0,-1)).splice(last(altP),1,...alt.lanes[0])}
  });
  select(null);
}
function delNode(){
  const p=sel.p;
  commit(()=>{seqAt(p.slice(0,-1)).splice(last(p),1)});
  select({k:'s',p:p});
}
function setRep(r){
  const n=nodeAt(sel.p);
  if(r==='n'){
    ui.cust=true;
    if(repKey(n)==='1')commit(()=>{n.min=2;n.max=3});
  }else{
    ui.cust=false;
    commit(()=>{const m={'1':[1,1],'?':[0,1],'*':[0,null],'+':[1,null]}[r];n.min=m[0];n.max=m[1]});
  }
  syncPanel();
}

/* ---------- panel ---------- */
const chip=(attr,inner,title,cls)=>`<button class="b ${cls||''}" ${attr} title="${title}" aria-label="${title}">${inner}</button>`;
function syncPanel(focusVal){
  if(!sel){panel.hidden=true;panel.innerHTML='';return}
  panel.hidden=false;
  if(sel.k==='s'){
    const p=sel.p, seq=seqAt(p.slice(0,-1)), inLane=p.length>=3;
    const canRm=inLane&&seq.length===0&&p[p.length-2]>=0&&nodeAt(p.slice(0,-2)).lanes.length>1;
    if(!canRm){panel.hidden=true;panel.innerHTML='';return}
    panel.innerHTML='<div class="row">'+chip('data-do="rmlane"',ICON.x,'Remove this lane','danger')+'</div>';
    return;
  }
  const n=nodeAt(sel.p), k=repKey(n), custom=ui.cust||k==='n';
  let h='';
  if(n.t==='look'){
    const ng=n.neg!==false;
    panel.innerHTML='<div class="row"><div class="seg">'+
      `<button class="b ic${ng?' on':''}" data-look="neg" title="Red: stop if this matches" aria-label="Red signal">${ICON.sigR}</button>`+
      `<button class="b ic${ng?'':' on'}" data-look="pos" title="Green: go only if this matches" aria-label="Green signal">${ICON.sigG}</button></div>`+
      '<div class="seg">'+
      `<button class="b ic${n.behind?'':' on'}" data-look="ahead" title="Watch ahead (lookahead)" aria-label="Watch ahead">${ICON.right}</button>`+
      `<button class="b ic${n.behind?' on':''}" data-look="behind" title="Watch behind (lookbehind)" aria-label="Watch behind">${ICON.left}</button></div>`+
      '<span class="sp"></span>'+chip('data-do="del"',ICON.trash,'Delete','danger')+'</div>';
    return;
  }
  if(n.t==='lit'||n.t==='set'){
    h+=`<div class="row"><input id="val" class="txt" type="text" value="${esc(n.v)}" autocomplete="off" autocapitalize="off" autocorrect="off" spellcheck="false" aria-label="${n.t==='lit'?'Text':'Character set'}">`+
      (n.t==='set'?`<button class="b${n.neg?' on':''}" data-do="neg" title="Negate (anything but)" aria-label="Negate">^</button>`:'')+'</div>';
  }
  h+='<div class="row"><div class="seg">'+
    ['1','?','*','+'].map(r=>`<button class="b${k===r&&!custom?' on':''}" data-rep="${r}">${r}</button>`).join('')+
    `<button class="b${custom?' on':''}" data-rep="n">{ }</button></div>`+
    (k!=='1'&&(n.max===null||n.min!==n.max)?`<button class="b ic${n.lazy?' on':''}" data-do="lazy" title="Switch: exit first (lazy)" aria-label="Exit first (lazy)" aria-pressed="${!!n.lazy}">${ICON.lazy}</button>`:'')+
    `<span class="sp"></span>`+
    (n.t==='alt'?chip('data-do="addlane"',ICON.plus,'Add lane'):chip('data-do="wrap"',ICON.fork,'Make a branch'))+
    chip('data-do="del"',ICON.trash,'Delete','danger')+'</div>';
  if(custom){
    h+=`<div class="row"><input id="rmin" class="num" type="number" min="0" max="999" inputmode="numeric" value="${n.min}" aria-label="Minimum">`+
      `<span class="dash">–</span><input id="rmax" class="num" type="number" min="1" max="999" inputmode="numeric" placeholder="∞" value="${n.max===null?'':n.max}" aria-label="Maximum"></div>`;
  }
  panel.innerHTML=h;
  if(focusVal){const v=$('#val');if(v){v.focus();v.select()}}
}
panel.addEventListener('click',e=>{
  const b=e.target.closest('button');
  if(!b||!sel)return;
  if(b.dataset.rep!==undefined)return setRep(b.dataset.rep);
  if(b.dataset.look){
    const n=nodeAt(sel.p), v=b.dataset.look;
    commit(()=>{if(v==='neg')n.neg=true;else if(v==='pos')n.neg=false;else n.behind=v==='behind'});
    return syncPanel();
  }
  const d=b.dataset.do;
  if(d==='lazy'){const n=nodeAt(sel.p);commit(()=>{if(n.lazy)delete n.lazy;else n.lazy=true});return syncPanel()}
  if(d==='del')delNode();
  else if(d==='wrap')wrapNode(sel.p);
  else if(d==='addlane')addLane(sel.p);
  else if(d==='rmlane')rmLane();
  else if(d==='neg'){const n=nodeAt(sel.p);commit(()=>{n.neg=!n.neg});syncPanel()}
});
panel.addEventListener('input',e=>{
  const t=e.target;
  if(!sel||sel.k!=='n')return;
  const n=nodeAt(sel.p);
  if(!editing){push();editing=true}
  if(t.id==='val')n.v=t.value;
  else if(t.id==='rmin')n.min=t.value===''?0:Math.max(0,Math.min(999,parseInt(t.value,10)||0));
  else if(t.id==='rmax')n.max=t.value===''?null:Math.max(1,Math.min(999,parseInt(t.value,10)||1));
  else return;
  save();update();
});
panel.addEventListener('focusout',()=>{editing=false});
panel.addEventListener('keydown',e=>{if(e.key==='Enter'&&e.target.id==='val')e.target.blur()});

/* ---------- canvas events ---------- */
function act(g){
  const a=g.dataset.a, p=parse(g.dataset.p||'');
  if(a==='start')commit(()=>{S.a=!S.a});
  else if(a==='goal')commit(()=>{S.z=!S.z});
  else if(a==='n')select({k:'n',p});
  else if(a==='s')select({k:'s',p});
  else if(a==='addlane')addLane(p);
}
let drag=null,lastDrag=0,asT=0,lastE=null,curT=null;
stage.addEventListener('click',e=>{
  if(Date.now()-lastDrag<300)return;
  const g=e.target.closest('[data-a]');
  if(!g||!svg.contains(g)){if(sel)select(null);return}
  act(g);
});
stage.addEventListener('keydown',e=>{
  if(e.key!=='Enter'&&e.key!==' ')return;
  const g=e.target.closest&&e.target.closest('[data-a]');
  if(!g||!svg.contains(g))return;
  e.preventDefault();act(g);
});

/* ---------- header / output ---------- */
tidyBtn.addEventListener('click',()=>{
  const root=tidy(JSON.parse(JSON.stringify(S.root)));
  if(JSON.stringify(root)===JSON.stringify(S.root))return;
  sel=null;commit(()=>{S.root=root});syncPanel();
});
undoBtn.addEventListener('click',()=>{
  const j=hist.pop();
  if(!j)return;
  const o=JSON.parse(j);S={a:!!o.a,z:!!o.z,root:clean(o.root)};
  sel=null;undoBtn.disabled=!hist.length;save();update();syncPanel();
});
clearBtn.addEventListener('click',()=>{
  commit(()=>{S={a:false,z:false,root:[]}});
  select({k:'s',p:[0]});
});
let flashT=0;
copyBtn.addEventListener('click',async()=>{
  const pat=pattern();
  if(!pat)return;
  try{await navigator.clipboard.writeText(pat)}
  catch(e){
    const r=document.createRange();r.selectNodeContents(out);
    const s=getSelection();s.removeAllRanges();s.addRange(r);
    try{document.execCommand('copy')}catch(_){}
  }
  copyBtn.innerHTML=ICON.check;clearTimeout(flashT);
  flashT=setTimeout(()=>{copyBtn.innerHTML=ICON.copy},1200);
});
tin.addEventListener('input',()=>{save();drawOut()});

/* ---------- drag & drop: stations go from the tray to the rail, and stay movable ---------- */
tray.addEventListener('click',e=>{
  const b=e.target.closest('.tk');
  if(!b||Date.now()-lastDrag<300)return;
  tapAdd(b.dataset.k);
});
const inBox=(e,r)=>e.clientX>=r.left&&e.clientX<=r.right&&e.clientY>=r.top&&e.clientY<=r.bottom;
function hit(e){
  if(drag.info.src==='rail')return railHit(e);
  if(inBox(e,tray.getBoundingClientRect()))return drag.info.src==='node'?{del:true}:null;
  if(!inBox(e,stage.getBoundingClientRect()))return null;
  const b=svg.getBoundingClientRect(), x=e.clientX-b.left, y=e.clientY-b.top;
  if(drag.info.src==='tray'&&drag.info.kind!=='look'){
    const nd=GEO.nodes.find(n=>n.t!=='alt'&&x>=n.x-6&&x<=n.x+n.w+6&&Math.abs(y-n.y)<=n.h/2+6);
    if(nd)return {wrap:nd.p};
  }
  let best=null,bd=1e9;
  GEO.lanes.forEach(l=>{const d=Math.hypot(x-l.cx,y-l.cy);if(d<22&&d<bd){bd=d;best={lane:l.p}}});
  if(best)return best;
  GEO.slots.forEach(s=>{const d=Math.hypot(x-s.cx,(y-s.cy)*1.25);if(d<bd){bd=d;best={slot:s.p}}});
  return bd<90?best:null;
}
function showMark(t){
  let mk=svg.querySelector('#mk');
  if(!mk){mk=document.createElementNS('http://www.w3.org/2000/svg','g');mk.id='mk';mk.setAttribute('pointer-events','none');svg.appendChild(mk)}
  let h='';
  if(t&&t.rail)h=railMark(t);
  else if(t&&t.slot){const s=GEO.slots.find(q=>pstr(q.p)===pstr(t.slot));if(s)h=`<rect class="mkb" x="${s.cx-2.5}" y="${s.cy-23}" width="5" height="46" rx="2.5"/>`}
  else if(t&&t.wrap){const n=GEO.nodes.find(q=>pstr(q.p)===pstr(t.wrap));if(n)h=`<rect class="mkw" x="${n.x-6}" y="${n.y-n.h/2-6}" width="${n.w+12}" height="${n.h+12}" rx="11"/>`}
  else if(t&&t.lane){const l=GEO.lanes.find(q=>pstr(q.p)===pstr(t.lane));if(l)h=`<circle class="mkl" cx="${l.cx}" cy="${l.cy}" r="13"/>`}
  mk.innerHTML=h;
  tray.classList.toggle('del',!!(t&&t.del));
}
function beginDrag(){
  drag.on=true;
  if(drag.info.src==='rail'){
    drag.a=GEO.slots.find(s=>pstr(s.p)===pstr(drag.info.p));
    svg.classList.add('dragging');
    asT=setInterval(autoScroll,30);
    return;
  }
  const g=document.createElement('div');
  if(drag.info.src==='tray'){g.className=drag.el.className+' dg';g.innerHTML=drag.el.innerHTML}
  else{
    const n=nodeAt(drag.info.path);
    g.className='tk dg '+n.t;
    g.innerHTML=n.t==='alt'?ICON.fork:n.t==='look'?ICON.signal:esc(labelOf(n));
    drag.el.classList.add('lift');
    tray.classList.add('mv');
  }
  document.body.appendChild(g);drag.ghost=g;
  svg.classList.add('dragging');
  asT=setInterval(autoScroll,30);
}
function autoScroll(){
  if(!drag||!drag.on||!lastE)return;
  const r=stage.getBoundingClientRect(),e=lastE,m=44;
  if(e.clientY<r.top-70||e.clientY>r.bottom+10)return;
  if(e.clientX<r.left+m&&e.clientX>r.left-60)stage.scrollLeft-=10;
  else if(e.clientX>r.right-m&&e.clientX<r.right+60)stage.scrollLeft+=10;
  if(e.clientY<r.top+m&&e.clientY>r.top-70)stage.scrollTop-=10;
  else if(e.clientY>r.bottom-m)stage.scrollTop+=10;
  curT=hit(e);showMark(curT);
}
function endDrag(cancel){
  clearInterval(asT);
  const t=curT,d=drag;
  drag=null;curT=null;
  if(d.ghost)d.ghost.remove();
  svg.classList.remove('dragging');
  tray.classList.remove('mv','del');
  const mk=svg.querySelector('#mk');if(mk)mk.remove();
  d.el.classList.remove('lift');
  lastDrag=Date.now();
  if(!cancel&&t)doDrop(d.info,t);
}
function doDrop(info,t){
  if(info.src==='rail')return doRail(t);
  if(info.src==='tray'){
    if(t.wrap)return branchWith(info.kind,t.wrap);
    if(t.slot)return placeNew(info.kind,seqAt(t.slot.slice(0,-1)),last(t.slot));
    if(t.lane){const n=mkNew(info.kind);commit(()=>{nodeAt(t.lane).lanes.push([n])});return afterAdd(n)}
    return;
  }
  const sp=info.path, node=nodeAt(sp), srcSeq=seqAt(sp.slice(0,-1)), si=last(sp);
  if(t.del){sel=null;commit(()=>{srcSeq.splice(si,1)});syncPanel();return}
  if(t.slot){
    const tSeq=seqAt(t.slot.slice(0,-1));let ti=last(t.slot);
    if(inside(node,tSeq))return;
    if(tSeq===srcSeq&&(ti===si||ti===si+1))return;
    commit(()=>{srcSeq.splice(si,1);if(tSeq===srcSeq&&si<ti)ti--;tSeq.splice(ti,0,node)});
  }else if(t.lane){
    const alt=nodeAt(t.lane);
    if(inside(node,alt))return;
    commit(()=>{srcSeq.splice(si,1);alt.lanes.push([node])});
  }else return;
  const p=findPath(node);
  select(p?{k:'n',p}:null);
}
/* 線路を引く: すき間から引っ張る。右上=迂回(省略)、右下=並行線(分岐)、左へ=戻り線(繰り返し) */
function bridge(x1,x2,y,s){
  const h=30,u=s<0;
  return `M${x1} ${y}a${R} ${R} 0 0 ${u?0:1} ${R} ${s*R}V${y+s*(h-R)}a${R} ${R} 0 0 ${u?1:0} ${R} ${s*R}H${x2-2*R}a${R} ${R} 0 0 ${u?1:0} ${R} ${-s*R}V${y+s*R}a${R} ${R} 0 0 ${u?0:1} ${R} ${-s*R}`;
}
function railMark(t){
  const x1=Math.min(t.ax,t.bx),x2=Math.max(t.ax,t.bx),y=t.y,back=t.bx<t.ax,s=(back||t.mode==='branch')?1:-1;
  let h=`<rect class="mkspan" x="${x1}" y="${y-24}" width="${x2-x1}" height="48" rx="10"/>`+
    `<path class="mkr" d="${bridge(x1,x2,y,s)}"/>`+
    `<circle class="mkdot" cx="${t.ax}" cy="${y}" r="5"/><circle class="mkdot" cx="${t.bx}" cy="${y}" r="5"/>`;
  if(back){const cx=(x1+x2)/2,cy=y+30;h+=`<path class="mkarw" d="M${cx+4} ${cy-4.5}L${cx-3} ${cy}L${cx+4} ${cy+4.5}z"/>`}
  return h;
}
function railHit(e){
  const a=drag.a;
  if(!a||!inBox(e,stage.getBoundingClientRect()))return null;
  const b=svg.getBoundingClientRect(), x=e.clientX-b.left, y=e.clientY-b.top;
  if(Math.abs(y-a.cy)>150||Math.abs(x-a.cx)<24)return null;
  const pre=pstr(a.p.slice(0,-1)), me=pstr(a.p);
  let best=null,bd=1e9;
  GEO.slots.forEach(s=>{
    if(pstr(s.p.slice(0,-1))!==pre||pstr(s.p)===me)return;
    const d=Math.abs(x-s.cx);
    if(d<bd){bd=d;best=s}
  });
  if(!best)return null;
  return {rail:true,a:a.p,b:best.p,mode:y>a.cy+14?'branch':'skip',ax:a.cx,bx:best.cx,y:a.cy};
}
function doRail(t){
  const seq=seqAt(t.a.slice(0,-1)), i1=last(t.a), i2=last(t.b), a=Math.min(i1,i2), b=Math.max(i1,i2);
  const span=seq.slice(a,b);
  if(!span.length)return;
  let res;
  if(i2<i1){
    if(span.length===1&&span[0].t!=='look'){res=span[0];commit(()=>{res.max=null})}
    else{res=mk('alt',{lanes:[span],max:null});commit(()=>seq.splice(a,b-a,res))}
  }else if(t.mode==='skip'){
    if(span.length===1&&span[0].t!=='look'){res=span[0];commit(()=>{res.min=0})}
    else{res=mk('alt',{lanes:[span],min:0});commit(()=>seq.splice(a,b-a,res))}
  }else{
    res=mk('alt',{lanes:[span,[]]});
    commit(()=>seq.splice(a,b-a,res));
    const q=findPath(res);
    return select(q?{k:'s',p:q.concat(1,0)}:null);
  }
  const p=findPath(res);
  select(p?{k:'n',p}:null);
}
document.addEventListener('pointerdown',e=>{
  if(e.button!==0||drag||!e.target.closest)return;
  let info=null,el=null;
  const tk=e.target.closest('.tk');
  if(tk&&tray.contains(tk)){info={src:'tray',kind:tk.dataset.k};el=tk}
  else{
    /* グループは枠(駅や線路のない所)をつかんで動かす */
    const b=e.target.closest('#rail .alt .frame');
    const g=b?b.closest('.alt'):e.target.closest('#rail .node');
    if(g&&g.dataset.p!==undefined){info={src:'node',path:parse(g.dataset.p)};el=g}
    else{
      const s=e.target.closest('#rail .slot:not(.empty)');
      if(s){info={src:'rail',p:parse(s.dataset.p)};el=s}
    }
  }
  if(!info)return;
  drag={info,el,sx:e.clientX,sy:e.clientY,on:false,id:e.pointerId,armed:true,timer:0};
  /* タッチでは長押しして初めて掴む。それまでは図を横スクロールできる */
  if(e.pointerType==='touch'&&info.src!=='tray'){
    const d=drag;
    d.armed=false;
    d.timer=setTimeout(()=>{
      if(drag!==d)return;
      d.armed=true;el.classList.add('armed');
      if(navigator.vibrate)try{navigator.vibrate(12)}catch(_){}
    },280);
  }
});
document.addEventListener('touchmove',e=>{if(drag&&(drag.armed||drag.on)&&e.cancelable)e.preventDefault()},{passive:false});
svg.addEventListener('contextmenu',e=>e.preventDefault());
document.addEventListener('pointermove',e=>{
  if(!drag||e.pointerId!==drag.id)return;
  lastE=e;
  if(!drag.on){
    const mv=Math.hypot(e.clientX-drag.sx,e.clientY-drag.sy);
    if(!drag.armed){if(mv>8){clearTimeout(drag.timer);drag=null}return}
    if(mv<6)return;
    beginDrag();
  }
  if(drag.ghost){drag.ghost.style.left=e.clientX+'px';drag.ghost.style.top=e.clientY+'px'}
  curT=hit(e);showMark(curT);
});
const dragEnd=e=>{
  if(!drag||e.pointerId!==drag.id)return;
  clearTimeout(drag.timer);drag.el.classList.remove('armed');
  if(drag.on)endDrag(e.type==='pointercancel');else drag=null;
};
document.addEventListener('pointerup',dragEnd);
document.addEventListener('pointercancel',dragEnd);

/* ---------- boot ---------- */
if(!load()){S=example();tin.value='cats, boxes, churches, potatoes, babies, boys, leaves, knives, a dog, the class'}
if(S.root.length===0)sel={k:'s',p:[0]};
update();syncPanel();
})();

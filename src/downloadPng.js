export default function downloadPng(canvasEl, name) {
  const url = canvasEl.toDataURL("image/png");
  const a = document.createElement("a");
  a.href = url;
  a.download = `splatter ${new Date()
    .toLocaleString()
    .replace(/\//g, "-")
    .replace(/:/g, ".")} seed ${name}.png`;
  a.click();

  // Clean up
  setTimeout(() => {
    URL.revokeObjectURL(url);
  }, 0);
}

// Handle GUI aspects of starring/unstarring a package.
function doStarSubmit() {
  st = document.getElementById("thestar");
  if (st.className == "icon-star-empty") {
    st.className = "icon-star-full";
    document.getElementById("starcount").innerHTML++;
  } else {
    st.className = "icon-star-empty";
    document.getElementById("starcount").innerHTML--;
  }
  st.parentNode.submit();
}

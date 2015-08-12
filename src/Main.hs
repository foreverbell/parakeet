import Render (renderFile)

main = do
  renderFile "../tests/Butter-fly/Butter-fly.j" "../tests/Butter-fly/Butter-fly.r" "Butter-fly.tex"
  renderFile "../tests/Anonymous/Anonymous.j" "../tests/Anonymous/Anonymous.r" "Anonymous.tex"

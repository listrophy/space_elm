namespace :assets do
  task :precompile do
    system("elm make --output=public/elm.js elm/Main.elm")
  end
end

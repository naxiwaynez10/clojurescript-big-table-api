(ns todo.core
  (:require [ajax.core :refer [GET]]
            [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.pprint :as pprint]
            [cljs.pprint :as p]))


(defonce api (r/atom {:loading? true :sort :id :dir true :index 0 :result 100 :us? true :search-keywords "" :search-key :city}))


(defn match
  "Search for matches and update the state"
  []
  (filter (fn [data]
            (if (:us? @api)
              (and (= "US" (:country_code data)) (str/includes? (str/lower-case ((:search-key @api) data)) (str (:search-keywords @api))))
              (and (not= "US" (:country_code data)) (str/includes? (str/lower-case ((:search-key @api) data)) (str (:search-keywords @api)))))) (:proxies (:data @api))))

(defn update-state
  "Updates the matches to state"
  []
  (let [m? (match)
        d (:result @api)
        i (count m?)
        [n step] (if (> d i) [i i] [d d])]
    (swap! api assoc :rows i :current (nth (partition n  step nil m?) (:index @api) m?))))


(defn get-api
  "Get the Todos from the API http://127.0.0.1:5500/malachie_large.json"
  []
  (GET "https://proxeidon.com/api_request.php"
    {:handler (fn [res]
                (swap! api assoc :data res)
                (update-state))
     :response-format :json
     :keywords? true}))




(defn sort-api
  "Sort the data in the API by ASCENDING or DESCENDING order"
  []
  (update-state)
  (if (:dir @api)
    (sort-by (:sort @api) > (:current @api))
    (sort-by (:sort @api) < (:current @api))))

(defn class
  "Arrow to show sort order"
  []
  (if (:dir @api) (str "fa-arrow-up") (str "fa-arrow-down")))


(defn active
  "Add active as a class to active th"
  [ref]
  (if (= (:sort @api) ref) (str "active") (str "")))

(defn action-button [params]
  (let [{id :id
         spam-o :spam-others
         spam-x :spam-xlb} params]
    [:button.btn.btn-primary
   {:data-bs-params
    (str id "-" spam-x "-" spam-o)
    :data-bs-target "#proxyM",
    :data-bs-toggle "modal",
    :type "button"}
   "Get"]))

(defn tr
  "Render the <tr> element for each todos"
  [item]
  (swap! api assoc :i (inc (:i @api)))
  (let [{ip :public_ip
         id :proxy_id
         type :proxy_type
         city :city
         country :country
         state :region
         ping :ping
         isp :isp
         zip :zip_code
         connection :conn_type
         spam-others :spam_others
         spam-xlb :spam_xbl} item]
    [:tr {:key id}
     [:td ip]
     [:td type]
     [:td country]
     [:td city]
     [:td state]
     [:td zip]
     [:td connection]
     [:td isp]
     [:td ping]
     [:td [action-button {:id id :spam-xlb spam-xlb :spam-others spam-others}]]]))


(defn search-input
  "Search inputs"
  [data]
  ;; Reset the results first 
  (let [{field :field
         placeholder :placeholder
         select? :select} data]
    (if select?
      [:select.form-control
       {:on-change #(swap! api assoc :search-keywords (-> % .-target .-value) :search-key field)}
       (case field
         :conn_type [:<> [:option {:value "wifi"} "WIFI"] [:option {:value "lte"} "LTE"]]
         :proxy_type [:<> [:option {:value "https"} "HTTPS"] [:option {:value "socks"} "SOCKS5"]])]
      [:input.form-control {:on-change #(swap! api assoc :search-keywords (-> % .-target .-value) :search-key field)
                            :placeholder placeholder :type "text"}])))

(defn pagination []
  (let [i (:rows @api)
        result (:result @api)
        steps (if (= (mod i result) 0)
                (/ i result)
                (+ 1 (int (/ i result))))]
    [:div.row
     [:div.col-12.mx-3
      [:nav {:aria-label "Page navigation example"}
       [:ul {:class "pagination"}
        [:li (if (= 0 (:index @api))  {:class "page-item  disabled"} {:class "page-item"})
         [:button {:class "page-link"
                   :on-click #(swap! api assoc :index (- (:index @api) 1))} "Previous"]]
        (map (fn [x]
               [:li {:class (if (= (:index @api) x) (str "page-item active") (str "page-item")) :key x}
                [:button {:class "page-link"
                          :key x
                          :on-click #(swap! api assoc :index x)} (+ 1 x)]]) (range steps))

        [:li (if (= (- steps 1) (:index @api))  {:class "page-item  disabled"} {:class "page-item"})
         [:button {:class "page-link"
                   :on-click #(swap! api assoc :index (+ (:index @api) 1))} "Next"]]]]]]))


(defn table-head []
  [:thead
   [:tr
    [:th {:scope "col"
          :class (active :public_ip)} [:span
                                       {:on-click #(swap! api assoc :sort :public_ip :dir (not (:dir @api)))} "Public IP  " [:i.fa {:class (class)}]]
     (search-input {:field :public_ip :placeholder "Public IP Address"})]
    [:th {:scope "col"
          :class (active :proxy_type)} [:span
                                        {:on-click #(swap! api assoc :sort :proxy_type :dir (not (:dir @api)))} "Protocol " [:i.fa {:class (class)}]]
     (search-input {:field :proxy_type :select true})]
    [:th {:scope "col"
          :class (active :country)} [:span
                                     {:on-click #(swap! api assoc :sort :country :dir (not (:dir @api)))} "Country " [:i.fa {:class (class)}]]
     (search-input {:field :country :placeholder "Country"})]
    [:th {:scope "col"
          :class (active :city)} [:span
                                  {:on-click #(swap! api assoc :sort :city :dir (not (:dir @api)))} "City " [:i.fa {:class (class)}]]
     (search-input {:field :city :placeholder "City"})]
    [:th {:scope "col"
          :class (active :region)} [:span
                                    {:on-click #(swap! api assoc :sort :region :dir (not (:dir @api)))} "State  " [:i.fa {:class (class)}]]
     (search-input {:field :region :placeholder "State"})]
    [:th {:scope "col"
          :class (active :zip_code)
          :on-click #(swap! api assoc :sort :zip_code :dir (not (:dir @api)))} "ZIP " [:i.fa {:class (class)}]]
    [:th {:scope "col"
          :class (active :conn_type)}
     [:span {:on-click #(swap! api assoc :sort :conn_type :dir (not (:dir @api)))} "Connect " [:i.fa {:class (class)}]]
     (search-input {:field :conn_type :select true})]
    [:th {:scope "col"
          :class (active :isp)}
     [:span {:on-click #(swap! api assoc :sort :isp :dir (not (:dir @api)))} "ISP " [:i.fa {:class (class)}]]
     (search-input {:field :isp :placeholder "isp"})]
    [:th {:scope "col"
          :class (active :ping)
          :on-click #(swap! api assoc :sort :ping :dir (not (:dir @api)))} "Ping " [:i.fa {:class (class)}]]
    [:th {:scope "col"} "Action"]]])



(defn table []
  [:table {:class ".mt-5 table"}
   [:caption "All US premium proxy list"]
   [table-head]
   [:tbody
    (when (empty? (:data @api))
      [:tr [:td {:colSpan 8} [:div.loader]]]) 
    (if (and (not (empty? (:data @api))) (empty? (:current @api))) [:tr [:td {:colSpan 8 :style {:text-align "center" :padding "8px"}} "No Results where found!"]])
    (map (fn [item] (tr item)) (sort-api))]])

(defn tabs []
  [:ul.nav.nav-pills.justify-content-center
   {:role "tablist"}
   [:li.nav-item
    {:role "presentation"
     :on-click #(swap! api assoc :us? true)}
    [:a
     {:aria-selected "true"
      :class (if (:us? @api) "nav-link active" "nav-link")
      :aria-controls "us"
      :role "tab"
      :href "#"
      :data-bs-toggle "tab"}
     "US"]]
   [:li
    {:role "presentation"
     :on-click #(swap! api assoc :us? false)}
    [:a
     {:aria-selected "false"
      :class (if-not (:us? @api) "nav-link active" "nav-link")
      :aria-controls "ww"
      :role "tab"
      :href "#"
      :data-bs-toggle "tab"}
     "World Wide"]]])

(defn heading []
  [:div.card-heading.text-center.pt-3 [:h2 "All US premium proxy list"]
   [:div.form-group
    [:div.row
     [:div.col-10
      [tabs]]
     [:div.col-2
      [:div.form-group
       [:label {:for "select"} "Select Range of result"]
       [:select.form-control
        {:on-change #(swap! api assoc :result (-> % .-target .-value int))}
        [:option {:value (:result @api)} (:result @api)]
        [:option {:value "10"} 10]
        [:option {:value "20"} 20]
        [:option {:value "50"} 50]
        [:option {:value "100"} 100]
        [:option {:value "200"} 200]
        [:option {:value "500"} 500]]
       [:span.pt-3 (str (:rows @api)) [:i " total results found"]]]]]]])

(defn main []
  (r/create-class
   {:component-will-mount (fn [_] (get-api))
    :component-did-update (fn [_ _] (update-state))
    :reagent-render (fn []
                      [:div.card.mt-5
                       [heading]
                       [:div.card-body
                        [table]
                        (pagination)]])}))


(defn ^:dev/after-load init! []
  (rdom/render [main] (.getElementById js/document "app")))

;; (init!)